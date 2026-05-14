package amyc
package interpreter

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier
import analyzer.SymbolTable

// An interpreter for Amy programs, implemented in Scala
object Interpreter extends Pipeline[(Program, SymbolTable), Unit]:

  // A class that represents a value computed by interpreting an expression
  enum Value:
    def asInt: Int = this.asInstanceOf[IntValue].i
    def asBoolean: Boolean = this.asInstanceOf[BooleanValue].b
    def asString: String = this.asInstanceOf[StringValue].s

    override def toString: String = this match
      case IntValue(i) => i.toString
      case BooleanValue(b) => b.toString
      case StringValue(s) => s
      case UnitValue => "()"
      case CaseClassValue(constructor, args) =>
        constructor.name + "(" + args.map(_.toString).mkString(", ") + ")"

    case IntValue(i: Int)
    case BooleanValue(b: Boolean)
    case StringValue(s: String)
    case UnitValue
    case CaseClassValue(constructor: Identifier, args: List[Value])
  
  import Value._

  def run(ctx: Context)(v: (Program, SymbolTable)): Unit =
    val (program, table) = v

    // These built-in functions do not have an Amy implementation in the program,
    // instead their implementation is encoded in this map
    val builtIns: Map[(String, String), (List[Value]) => Value] = Map(
      ("Std", "printInt")    -> { args => println(args.head.asInt); UnitValue },
      ("Std", "printString") -> { args => println(args.head.asString); UnitValue },
      ("Std", "readString")  -> { args => StringValue(scala.io.StdIn.readLine()) },
      ("Std", "readInt")     -> { args =>
        val input = scala.io.StdIn.readLine()
        try
          IntValue(input.toInt)
        catch
          case ne: NumberFormatException =>
            ctx.reporter.fatal(s"""Could not parse "$input" to Int""")
      },
      ("Std", "intToString")   -> { args => StringValue(args.head.asInt.toString) },
      ("Std", "digitToString") -> { args => StringValue(args.head.asInt.toString) }
    )

    // Utility functions to interface with the symbol table.
    def isConstructor(name: Identifier) = table.getConstructor(name).isDefined
    def findFunctionOwner(functionName: Identifier) = table.getFunction(functionName).get.owner.name
    def findFunction(owner: String, name: String) =
      program.modules.find(_.name.name == owner).get.defs.collectFirst:
        case fd@FunDef(fn, _, _, _) if fn.name == name => fd
      .get

    // Interprets a function, using evaluations for local variables contained in 'locals'
    // TODO: Complete all missing cases. Look at the given ones for guidance. 
    def interpret(expr: Expr)(implicit locals: Map[Identifier, Value]): Value =
      expr match
        case Variable(name) =>
          locals.getOrElse(name, ctx.reporter.fatal(s"Variable not found: $name"))
        case IntLiteral(i) =>
          IntValue(i)
        case BooleanLiteral(b) =>
          BooleanValue(b)
        case StringLiteral(s) =>
          StringValue(s)
        case UnitLiteral() =>
          UnitValue
        case Plus(lhs, rhs) =>
          IntValue(interpret(lhs).asInt + interpret(rhs).asInt)

        case Minus(lhs, rhs) =>
          IntValue(interpret(lhs).asInt - interpret(rhs).asInt)

        case Times(lhs, rhs) =>
          IntValue(interpret(lhs).asInt * interpret(rhs).asInt)

        case Div(lhs, rhs) =>
          IntValue(interpret(lhs).asInt / interpret(rhs).asInt)

        case Mod(lhs, rhs) =>
          IntValue(interpret(lhs).asInt % interpret(rhs).asInt)

        case LessThan(lhs, rhs) =>
          BooleanValue(interpret(lhs).asInt < interpret(rhs).asInt)

        case LessEquals(lhs, rhs) =>
          BooleanValue(interpret(lhs).asInt <= interpret(rhs).asInt)

        case And(lhs, rhs) =>
          if (interpret(lhs).asBoolean) interpret(rhs) else BooleanValue(false)

        case Or(lhs, rhs) =>
          if (interpret(lhs).asBoolean) BooleanValue(true) else BooleanValue(interpret(rhs).asBoolean)

        case Equals(lhs, rhs) =>
          BooleanValue(interpret(lhs) == interpret(rhs))
          // Hint: Take care to implement Amy equality semantics
        case Concat(lhs, rhs) =>
          StringValue(interpret(lhs).asString + interpret(rhs).asString)
        case Not(e) =>
          BooleanValue(!interpret(e).asBoolean)

        case Neg(e) =>
          IntValue(-interpret(e).asInt)

        case Call(qname, args) =>
          // Hint: Check if it is a call to a constructor first,
          // then if it is a built-in function (otherwise it is a normal function).
          // Use the helper methods provided above to retrieve information from the symbol table.
          // Think how locals should be modified.
          val evalArgs = args.map(arg => interpret(arg))
          // Check if it is a call to a constructor first
          if isConstructor(qname) then
            CaseClassValue(qname, evalArgs)
          else
            // check if built-in function, otherwise normal function
            val owner = findFunctionOwner(qname)
            val key = (owner, qname.name)
            builtIns.get(key) match
              case Some(impl) =>
                impl(evalArgs)
              case None =>
                val funcDef = findFunction(owner, qname.name)
                val updatedLocals = funcDef.params.map(_.name).zip(evalArgs).toMap
                interpret(funcDef.body)(using updatedLocals)
          
        case Sequence(e1, e2) =>
          // We get effect of e1, but discard its value, and return the value of e2
          interpret(e1)
          interpret(e2)

        case Let(df, value, body) =>
          val evalVal = interpret(value)
          interpret(body)(using locals + (df.name -> evalVal))

        case Ite(cond, thenn, elze) =>
          if interpret(cond).asBoolean then interpret(thenn)
          else interpret(elze)


       // ====================================== PATTERN MATCHING  ============================================
        case Match(scrut, cases) => 
          val evS = interpret(scrut) // Helper function to check if a value matches a pattern, and if so return the bindings of variables in the pattern to their corresponding values
          // None = pattern does not match
          // Returns a list of pairs id -> value,
          // where id has been bound to value within the pattern.
          // Returns None when the pattern fails to match.
          // Note: Only works on well typed patterns (which have been ensured by the type checker).

          // Main "loop" of the implementation: Go through every case,
          // check if the pattern matches, and if so return the evaluation of the case expression with the correct bindings for the pattern variables.

          def matchesPattern(v: Value, pat: Pattern): Option[List[(Identifier, Value)]] = 
            ((v, pat): @unchecked) match
              case (_, WildcardPattern()) => 
                Some(Nil)
              case (_, IdPattern(name)) => 
                Some(List((name, v)))
              case (IntValue(i1), LiteralPattern(IntLiteral(i2))) =>
                  if i1 == i2 then Some(Nil) else None
              case (BooleanValue(b1), LiteralPattern(BooleanLiteral(b2))) =>
                  if b1 == b2 then Some(Nil) else None
              case (StringValue(s1), LiteralPattern(StringLiteral(s2))) =>
                  if s1 == s2 then Some(Nil) else None
              case (UnitValue, LiteralPattern(UnitLiteral())) =>
                  Some(Nil)
              case (CaseClassValue(con1, realArgs), CaseClassPattern(con2, formalArgs)) =>
                  if con1 != con2 || realArgs.length != formalArgs.length then
                    None
                  else
                    realArgs.zip(formalArgs).foldLeft(Option(List.empty[(Identifier, Value)])):
                      case (Some(acc), (realArg, formalArg)) =>
                        matchesPattern(realArg, formalArg) match
                          case Some(bindings) => Some(acc ++ bindings)
                          case None => None
                      case (None, _) =>
                        None

          cases.to(LazyList).map(matchCase => 
            val MatchCase(pat, rhs) = matchCase
            (rhs, matchesPattern(evS, pat))
          ).find(_._2.isDefined) match
            case Some((rhs, Some(moreLocals))) =>
              interpret(rhs)(using locals ++ moreLocals)
            case _ =>
              // No case matched
              ctx.reporter.fatal(s"Match error: ${evS.toString}@${scrut.position}")


        case Error(msg) =>
          ctx.reporter.fatal(interpret(msg).asString)

    end interpret

    // The entry point of the interpreter: Looks for the main expression in the program, and interprets it.
    for
      m <- program.modules
      e <- m.optExpr
    do
      interpret(e)(using Map())
    
  end run
