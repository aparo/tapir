package tapir.typelevel

import scala.annotation.implicitNotFound

/**
  * View parameters (single type or a tuple) as arguments of a function of the appropriate arity.
  */
trait ParamsAsArgs[I] {
  // FN[R] = (I as args) => R
  type FN[_]

  def toFn[O](f: I => O): FN[O]
  def paramAt(params: I, i: Int): Any
  def applyFn[R](f: FN[R], args: I): R
}

object ParamsAsArgs extends LowPriorityParamsAsArgs1 {

  implicit def tuple2ToFn[A1, A2]: Aux[(A1, A2), (A1, A2) => ?] = new ParamsAsArgs[(A1, A2)] {
    type FN[O] = (A1, A2) => O
    override def toFn[R](f: ((A1, A2)) => R): (A1, A2) => R = (a1, a2) => f((a1, a2))
    override def paramAt(params: (A1, A2), i: Int): Any = params.productElement(i)
    override def applyFn[R](f: (A1, A2) => R, args: (A1, A2)): R = f(args._1, args._2)
  }

  implicit def tuple3ToFn[A1, A2, A3]: Aux[(A1, A2, A3), (A1, A2, A3) => ?] = new ParamsAsArgs[(A1, A2, A3)] {
    type FN[O] = (A1, A2, A3) => O
    override def toFn[R](f: ((A1, A2, A3)) => R): (A1, A2, A3) => R = (a1, a2, a3) => f((a1, a2, a3))
    override def paramAt(params: (A1, A2, A3), i: Int): Any = params.productElement(i)
    override def applyFn[R](f: (A1, A2, A3) => R, args: (A1, A2, A3)): R = f(args._1, args._2, args._3)
  }

  implicit def tuple4ToFn[A1, A2, A3, A4]: Aux[(A1, A2, A3, A4), (A1, A2, A3, A4) => ?] = new ParamsAsArgs[(A1, A2, A3, A4)] {
    type FN[O] = (A1, A2, A3, A4) => O
    override def toFn[R](f: ((A1, A2, A3, A4)) => R): (A1, A2, A3, A4) => R = (a1, a2, a3, a4) => f((a1, a2, a3, a4))
    override def paramAt(params: (A1, A2, A3, A4), i: Int): Any = params.productElement(i)
    override def applyFn[R](f: (A1, A2, A3, A4) => R, args: (A1, A2, A3, A4)): R = f(args._1, args._2, args._3, args._4)
  }

  implicit def tuple5ToFn[A1, A2, A3, A4, A5]: Aux[(A1, A2, A3, A4, A5), (A1, A2, A3, A4, A5) => ?] =
    new ParamsAsArgs[(A1, A2, A3, A4, A5)] {
      type FN[O] = (A1, A2, A3, A4, A5) => O
      override def toFn[R](f: ((A1, A2, A3, A4, A5)) => R): (A1, A2, A3, A4, A5) => R = (a1, a2, a3, a4, a5) => f((a1, a2, a3, a4, a5))
      override def paramAt(params: (A1, A2, A3, A4, A5), i: Int): Any = params.productElement(i)
      override def applyFn[R](f: (A1, A2, A3, A4, A5) => R, args: (A1, A2, A3, A4, A5)): R = f(args._1, args._2, args._3, args._4, args._5)
    }

  implicit def tuple6ToFn[A1, A2, A3, A4, A5, A6]: Aux[(A1, A2, A3, A4, A5, A6), (A1, A2, A3, A4, A5, A6) => ?] =
    new ParamsAsArgs[(A1, A2, A3, A4, A5, A6)] {
      type FN[O] = (A1, A2, A3, A4, A5, A6) => O
      override def toFn[R](f: ((A1, A2, A3, A4, A5, A6)) => R): (A1, A2, A3, A4, A5, A6) => R =
        (a1, a2, a3, a4, a5, a6) => f((a1, a2, a3, a4, a5, a6))
      override def paramAt(params: (A1, A2, A3, A4, A5, A6), i: Int): Any = params.productElement(i)
      override def applyFn[R](f: (A1, A2, A3, A4, A5, A6) => R, args: (A1, A2, A3, A4, A5, A6)): R =
        f(args._1, args._2, args._3, args._4, args._5, args._6)
    }
}

trait LowPriorityParamsAsArgs1 extends LowPriorityParamsAsArgs0 {
  implicit def unitToFn: Aux[Unit, Function0] = new ParamsAsArgs[Unit] {
    type FN[O] = () => O
    override def toFn[O](f: Unit => O): () => O = () => f(())
    override def paramAt(params: Unit, i: Int): Any = throw new IndexOutOfBoundsException(i.toString)
    override def applyFn[R](f: () => R, args: Unit): R = f()
  }
}

trait LowPriorityParamsAsArgs0 {
  @implicitNotFound(msg = "Expected arguments: ${I}")
  type Aux[I, _FN[_]] = ParamsAsArgs[I] { type FN[O] = _FN[O] }

  implicit def singleToFn[A1]: Aux[A1, A1 => ?] = new ParamsAsArgs[A1] {
    type FN[O] = A1 => O
    override def toFn[R](f: A1 => R): A1 => R = a1 => f(a1)
    override def paramAt(params: A1, i: Int): Any = if (i == 0) params else throw new IndexOutOfBoundsException(i.toString)
    override def applyFn[R](f: A1 => R, args: A1): R = f(args)
  }
}
