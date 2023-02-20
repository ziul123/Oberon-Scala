package br.unb.cic.oberon.codegen

import br.unb.cic.oberon.ir.ast.{Constant => ASTConstant, _}
import br.unb.cic.oberon.ir.tac._
import br.unb.cic.oberon.tc.{ExpressionTypeVisitor, TypeChecker}

object TACodeGenerator extends CodeGenerator[List[TAC]] {
  
  private var tc = new TypeChecker()
  private var expVisitor = new ExpressionTypeVisitor(tc)

  override def generateCode(module: OberonModule): List[TAC] = {
    List()
  }

  def generateProcedure() {}

  def generateBody() {}

  def generateExpression(expr: Expression, insts: List[TAC]): (Address, List[TAC]) = {
    expr match {

      case Brackets(exp) =>
        return generateExpression(exp, insts)

      case IntValue(value) =>
        return (Constant(value.toString, IntegerType), insts)

      case RealValue(value) =>
        return (Constant(value.toString, RealType), insts)

      case CharValue(value) =>
        return (Constant(value.toString, CharacterType), insts)

      case BoolValue(value) =>
        return (Constant(value.toString, BooleanType), insts)

      case StringValue(value) =>
        return (Constant(value, StringType), insts)

      case NullValue =>
        return (Constant("Null", NullType), insts)

      case VarExpression(name) =>
        return (Name(name, expr.accept(expVisitor).get), insts)

      case AddExpression(left, right) =>
        val (temps, l, r, insts2) = generateBinaryAndComparisonExpression(expr.accept(expVisitor).get, left, right, insts)
        val (t) = temps(0)
        return (t, insts2 :+ AddOp(l, r, t, ""))

      case SubExpression(left, right) =>
        val (temps, l, r, insts2) = generateBinaryAndComparisonExpression(expr.accept(expVisitor).get, left, right, insts)
        val (t) = temps(0)
        return (t, insts2 :+ SubOp(l, r, t, ""))

      case MultExpression(left, right) =>
        val (temps, l, r, insts2) = generateBinaryAndComparisonExpression(expr.accept(expVisitor).get, left, right, insts)
        val (t) = temps(0)
        return (t, insts2 :+ MulOp(l, r, t, ""))

      case DivExpression(left, right) =>
        val (temps, l, r, insts2) = generateBinaryAndComparisonExpression(expr.accept(expVisitor).get, left, right, insts)
        val (t) = temps(0)
        return (t, insts2 :+ DivOp(l, r, t, ""))
        
      case AndExpression(left, right) =>
        val (temps, l, r, insts2) = generateBinaryAndComparisonExpression(BooleanType, left, right, insts)
        val (t) = temps(0)
        return (t, insts2 :+ AndOp(l, r, t, ""))

      case OrExpression(left, right) =>
        val (temps, l, r, insts2) = generateBinaryAndComparisonExpression(BooleanType, left, right, insts)
        val (t) = temps(0)
        return (t, insts2 :+ OrOp(l, r, t, ""))

      case ModExpression(left, right) =>
        val (temps, l, r, insts2) = generateBinaryAndComparisonExpression(IntegerType, left, right, insts)
        val (t) = temps(0)
        return (t, insts2 :+ RemOp(l, r, t, ""))

      case NotExpression(exp) =>
        val (a, insts1) = generateExpression(exp, insts)
        val t = new Temporary(BooleanType)
        return (t, insts1 :+ NotOp(a, t, ""))

      case EQExpression(left, right) =>
        val (temps, l, r, insts2) = generateBinaryAndComparisonExpression(expr, left, right, insts)
        val (t0, t1) = (temps(0), temps(1))
        return (t1, insts2 :+ SubOp(l, r, t0, "") :+ SLTUOp(t0, Constant("1", IntegerType), t1, ""))

      case NEQExpression(left, right) =>
        val (temps, l, r, insts2) = generateBinaryAndComparisonExpression(expr, left, right, insts)
        val (t0, t1) = (temps(0), temps(1))
        return (t1, insts2 :+ SubOp(l, r, t0, "") :+ SLTUOp(Constant("0", IntegerType), t0, t1, ""))

      case GTExpression(left, right) =>
        val (temps, l, r, insts2) = generateBinaryAndComparisonExpression(expr, left, right, insts)
        val t = temps(0)
        return (t, insts2 :+ SLTOp(r, l, t, ""))

      case LTExpression(left, right) =>
        val (temps, l, r, insts2) = generateBinaryAndComparisonExpression(expr, left, right, insts)
        val t = temps(0)
        return (t, insts2 :+ SLTOp(l, r, t, ""))

      case GTEExpression(left, right) =>
        val (temps, l, r, insts2) = generateBinaryAndComparisonExpression(expr, left, right, insts)
        val (t0, t1) = (temps(0), temps(1))
        return (t1, insts2 :+ SLTOp(l, r, t0, "") :+ NotOp(t0, t1, ""))

      case LTEExpression(left, right) =>
        val (temps, l, r, insts2) = generateBinaryAndComparisonExpression(expr, left, right, insts)
        val (t0, t1) = (temps(0), temps(1))
        return (t1, insts2 :+ SLTOp(r, l, t0, "") :+ NotOp(t0, t1, ""))

//TODO generateProcedure e gerar o Map funcs
//      case FunctionCallExpression(name, args) =>
//        val (args, argInsts) = exp.args.foldLeft((List[Address](),insts)) {
//          (acc, expr) => 
//            val (address, ops) = TACodeGenerator.generateExpression(expr, acc._2)
//            (acc._1 :+ address, ops)
//        }
//        val params = args.map(x => Param(x, ""))
//        //talvez mudar o funcs.get(name) para stack
//        return (funcs.get(name), argInsts ++ params :+ Call(name, args.length))

      case ArraySubscript(array, index) =>
        val (a, insts1) = generateExpression(array, insts)
        val (i, insts2) = generateExpression(index, insts1)
        val t = new Temporary(expr.accept(expVisitor).get)
        return (t, insts2 :+ ListGet(a, i, t, ""))

      case PointerAccessExpression(name) =>
        val p = Name(name, LocationType)
        val t = new Temporary(expr.accept(expVisitor).get)
        return (t, insts :+ GetValue(p, t, ""))

      case FieldAccessExpression(exp, name) =>
        throw new Exception("FieldAccessExpression não foi implementada!")
    }
  }

  def generateStatement() {}

  private def generateBinaryAndComparisonExpression(
    exprType: Optional[Type], expr: Optional[Expressions],
    left: Expression, right: Expression,
    insts: List[TAC]
  ): (List[Address], Address, Address, List[TAC]) {
    val (l, insts1) = generateExpression(left, insts)
    val (r, insts2) = generateExpression(right, insts1)

    val temps = exprType match {
      case Some(i) => List(new Temporary(i))
      case None => expr match {
        case GTExpression(_,_) | LTExpression(_,_) => List(new Temporary(BooleanType))
        case other => List(new Temporary(BooleanType), new Temporary(BooleanType))
      }
    }

    (temps, l, r, insts2)
  }

  // somente para testes
  def load_vars(vars: List[VariableDeclaration], consts: List[ASTConstant] = List()): Unit = {
    OberonModule("test", Set(), List(), consts, vars, List(), None).accept(tc);
  }
  // somente para testes
  def reset(): Unit = {
    tc = new TypeChecker()
    expVisitor = new ExpressionTypeVisitor(tc)
    Temporary.reset
  }
}
