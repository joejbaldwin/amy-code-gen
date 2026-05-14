package amyc
package analyzer

import amyc.utils._
import amyc.ast.SymbolicTreeModule._
import amyc.ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)]:

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) =
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: Type, expected: Type, pos: Position)

    // Represents a type variable.
    // It extends Type, but it is meant only for internal type checker use,
    //  since no Amy value can have such type.
    case class TypeVariable private (id: Int) extends Type
    object TypeVariable:
      private val c = new UniqueCounter[Unit]
      def fresh(): TypeVariable = TypeVariable(c.next(()))

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: Type)(implicit env: Map[Identifier, Type]): List[Constraint] =
      
      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] =
        List(Constraint(found, expected, e.position))
      
      e match
        case IntLiteral(_) =>
          topLevelConstraint(IntType)
        case Equals(lhs, rhs) =>
          // HINT: Take care to implement the specified Amy semantics
          // (A, A) => Bool
          val typeVar = TypeVariable.fresh()
          topLevelConstraint(BooleanType) ++
          genConstraints(lhs, typeVar) ++ 
          genConstraints(rhs, typeVar) 

        case BooleanLiteral(_) =>
          // true, false => BooleanType
          topLevelConstraint(BooleanType)

        case StringLiteral(_) => 
          // "hello" => StringType
          topLevelConstraint(StringType)

        case UnitLiteral() =>
          // () => UnitType
          topLevelConstraint(UnitType)

        case Variable(name) =>
          // value assigned in the current context
          topLevelConstraint(env(name))

        case Plus(lhs, rhs) =>
          // lhs: intType, rhs: intType
          topLevelConstraint(IntType) ++
          genConstraints(lhs, IntType) ++ 
          genConstraints(rhs, IntType)

        case Minus(lhs, rhs) =>
          // lhs: intType, rhs: intType
          topLevelConstraint(IntType) ++ 
          genConstraints(lhs, IntType) ++
          genConstraints(rhs, IntType)
        
        case Times(lhs, rhs) =>
          // lhs: intType, rhs: intType => intType
          topLevelConstraint(IntType) ++ 
          genConstraints(lhs, IntType) ++
          genConstraints(rhs, IntType)

        case Div(lhs, rhs) =>
          // lhs: intType, rhs: intType => intType
          topLevelConstraint(IntType) ++ 
          genConstraints(lhs, IntType) ++
          genConstraints(rhs, IntType)

        case Mod(lhs, rhs) =>
          // lhs: intType, rhs: intType => intType
          topLevelConstraint(IntType) ++ 
          genConstraints(lhs, IntType) ++
          genConstraints(rhs, IntType)

        case LessThan(lhs, rhs) =>
          // lhs: intType, rhs: intType => booleanType
          topLevelConstraint(BooleanType) ++
          genConstraints(lhs, IntType) ++
          genConstraints(rhs, IntType)

        case LessEquals(lhs, rhs) =>
          // lhs: intType, rhs: intType => booleanType
          topLevelConstraint(BooleanType) ++
          genConstraints(lhs, IntType) ++
          genConstraints(rhs, IntType)

        case And(lhs, rhs) =>
          // lhs: booleanType, rhs: booleanType => booleanType
          topLevelConstraint(BooleanType) ++
          genConstraints(lhs, BooleanType) ++
          genConstraints(rhs, BooleanType)

        case Or(lhs, rhs) =>
          // lhs: booleanType, rhs: booleanType => booleanType
          topLevelConstraint(BooleanType) ++
          genConstraints(lhs, BooleanType) ++
          genConstraints(rhs, BooleanType)

        case Not(e) =>
          // e: booleanType => booleanType
          topLevelConstraint(BooleanType) ++
          genConstraints(e, BooleanType)

        case Neg(e) =>
          // e: intType => intType
          topLevelConstraint(IntType) ++
          genConstraints(e, IntType)

        case Concat(lhs, rhs) =>
          topLevelConstraint(StringType) ++
          genConstraints(lhs, StringType) ++
          genConstraints(rhs, StringType)

        case Sequence(e1, e2) =>
          // (A, B) => B
          val A = TypeVariable.fresh()
          val B = TypeVariable.fresh()
          topLevelConstraint(B) ++
          genConstraints(e1, A) ++
          genConstraints(e2, B)

        case Error(msg) =>
          // string => T
          topLevelConstraint(expected) ++
          genConstraints(msg, StringType)

        case Ite(cond, thenn, elze) =>
          // cond: bool, thenn: T, elze: T => T
          val T = TypeVariable.fresh()
          topLevelConstraint(T) ++
          genConstraints(cond, BooleanType) ++
          genConstraints(thenn, T) ++
          genConstraints(elze, T)
        

        case Let(df, value, body) =>
          // Γ ⊢ e1 : T1   Γ, n : T1 ⊢ e2 : T2
          // Γ ⊢ val n : T1 = e1 ; e2 : T2
          val T1 = df.tt.tpe
          val newEnv = env + (df.name -> T1)
          topLevelConstraint(expected) ++
          genConstraints(value, T1) ++
          genConstraints(body, expected)(using newEnv)

        case Call(qname, args) =>
          val sig = table.getFunction(qname).orElse(table.getConstructor(qname)).get
          val argTypes = sig.argTypes
          val retType = sig.retType
          
          // Entire call is retType, each arg must have its own type
          topLevelConstraint(retType) ++
          args.zip(argTypes).flatMap { case (arg, t) => genConstraints(arg, t) }


        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def patternBindings(pat: Pattern, expected: Type): (List[Constraint], Map[Identifier, Type]) =
              pat match
                // we are retuning a tuple (constraint, new bindings)
                case WildcardPattern() => 
                  // no constraint, no new bindings
                  (Nil, Map.empty)
                case IdPattern(name) =>
                  // x should map to whatever type the scrut has (expected)
                  (Nil, Map(name -> expected))
                case LiteralPattern(lit) =>
                  (genConstraints(lit, expected), Map.empty)
                case CaseClassPattern(constr, args) =>
                  val sig = table.getConstructor(constr).get
                  val argTypes = sig.argTypes        
                  val retType = sig.retType          
                  // return type must mach the scrut type
                  val parentConstraint = Constraint(retType, expected, pat.position)
                  val subProbs: List[(List[Constraint], Map[Identifier, Type])] =
                    args.zip(argTypes).map { case (p, typ) => patternBindings(p, typ) }
                  // combine into one 
                  val subConstraints = subProbs.flatMap(_._1)
                  val subBindings = subProbs.map(_._2).reduceOption(_ ++ _).getOrElse(Map.empty)
                  (List(parentConstraint) ++ subConstraints, subBindings)

          def handleCase(cse: MatchCase, scrutExpected: Type): List[Constraint] =
            val (patConstraints, moreEnv) = patternBindings(cse.pat, scrutExpected)
            // Body must match the type of the scrut 
            val bodyConstraints = genConstraints(cse.expr, expected)(using env ++ moreEnv)
            patConstraints ++ bodyConstraints

          val st = TypeVariable.fresh()
          genConstraints(scrut, st) ++
          cases.flatMap(cse => handleCase(cse, st))

    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] =
      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }

    // Do a single substitution.
    def subst(tpe: Type, from: Int, to: Type): Type =
      tpe match
        case TypeVariable(`from`) => to
        case other => other

    // Solve the given set of typing constraints and report errors
    //  using `ctx.reporter.error` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit =
      constraints match
        case Nil => ()
        case Constraint(found, expected, pos) :: more =>
          (found, expected) match
            case (a, b) if a == b =>
              // equal case, just discard and dont create error
              solveConstraints(more)
            case (TypeVariable(n), other) =>
              // lhs is a type var, learn n = other
              solveConstraints(subst_*(more, n, other))
            case (other, TypeVariable(n)) =>
              // rhs is a type var, symmetric
              solveConstraints(subst_*(more, n, other))
            case _ =>
              // both concrete and unequal -> error
              error(s"Type mismatch: found $found, expected $expected", pos)
              solveConstraints(more)

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env = params.map { case ParamDef(name, tt) => name -> tt.tpe }.toMap
        solveConstraints(genConstraints(body, retType.tpe)(using env))
      }

      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraints(genConstraints(e, tv)(using Map())))
    }

    v
  end run

end TypeChecker
