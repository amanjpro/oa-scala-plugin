package ch.usi.inf.l3.oa.explang


import ch.usi.inf.l3.oa.quals._
import ch.usi.inf.l3.oa.library._
import scala.language.higherKinds
import scala.reflect._


//------------------------------------------------------------
// Values
//------------------------------------------------------------
trait Value
trait IntValue extends Value {
  def value: Int
  override def toString: String = value.toString
}


//------------------------------------------------------------
// Pseudo-AST
//------------------------------------------------------------
trait Exp

trait Lit extends Exp {
  def value: Int
}

trait Add extends Exp {
  def lhs: Exp 
  def rhs: Exp 
}



//------------------------------------------------------------
// The Algebra
//------------------------------------------------------------


@merger("ch.usi.inf.l3.oa.explang.AlgExp") trait ExpMerge
// Alt
// @merger trait ExpMerge extends AlgExp

// object ExpComb2 extends Algebra[AlgExp] {
//   def merge[A, B](f: TLifter[A, B], a1: AlgExp[A], a2: AlgExp[B]) = {
//     new ExpMerge2[A, B]() {
//       def lifter: TLifter[A, B] = f
//       def alg1: AlgExp[A] = a1
//       def alg2: AlgExp[B] = a2
//     }
//   }
//
//   // This can be overcome using some macro magic!
//   def empty = useEmpty[AlgExp[Any]]("ch.usi.inf.l3.oa.explang.ExpEmpty")
// }


// trait ExpMerge2[A, B] extends AlgExp[A with B] {
//   def alg1: AlgExp[A]
//   def alg2: AlgExp[B]
//   def lifter: TLifter[A, B]
//
//   def Lit(n: Int): A with B = lifter.lift(alg1.Lit(n), alg2.Lit(n))
//   def Add(l: A with B, r: A with B): A with B = 
//     lifter.lift(alg1.Add(l, r), alg2.Add(l, r))
// }


object ExpComb extends Algebra[AlgExp] {
  def merge[A, B](f: TLifter[A, B], a1: AlgExp[A], a2: AlgExp[B]) = {
    // mergeWith[AlgExp[A with B]]("ch.usi.inf.l3.oa.explang.ExpMerge")
    new ExpMerge {
      val alg1 = a1
      val alg2 = a2
      val lifter = f
    }
  }

  // This can be overcome using some macro magic!
  def empty = EmptyAlg
}


@empty("ch.usi.inf.l3.oa.explang.AlgExp") object ExpEmpty 

@lifter("ch.usi.inf.l3.oa.explang.IEval",
        "ch.usi.inf.l3.oa.explang.IShow") object LiftEP 


trait AlgExp[T] {
  def Lit(v: Int): T
  def Add(lhs: T, rhs: T): T
}


//------------------------------------------------------------
// Features
//------------------------------------------------------------
trait IEval {
  def eval(): Value
}


trait IShow {
  def show: String
}

trait AlgExpEval extends AlgExp[IEval] {
  def Lit(v: Int): IEval = {
    new IEval {
      def eval(): Value = new IntValue {val value = v}
    }
  }

  def Add(lhs: IEval, rhs: IEval): IEval = {
    new IEval {
      def eval(): Value = {
        val v1 = lhs.eval
        val v2 = rhs.eval
        (v1, v2) match {
          case (x: IntValue, y: IntValue) => 
            new IntValue { val value = x.value + y.value }
          case _ => throw new Exception("Error")
        }
      }
    }
  }
}

object AlgExpEval extends AlgExpEval

trait AlgExpShow extends AlgExp[IShow] {
  def Lit(v: Int): IShow = {
    new IShow {
      def show: String = v.toString
    }
  }

  def Add(lhs: IShow, rhs: IShow): IShow = {
    new IShow {
      def show: String = s"${lhs.show} + ${rhs.show}"
    }
  }
}
object AlgExpShow extends AlgExpShow


//------------------------------------------------------------
// Test
//------------------------------------------------------------
object OAT {
  def main(args: Array[String]): Unit = {
    // TODO: Fix this after the macro
    val lifter = useLifter[IEval, IShow]("ch.usi.inf.l3.oa.explang.LiftEP")
    val b = ExpComb.merge(lifter, AlgExpEval, AlgExpShow)
    // val b = ExpComb.merge(LiftEP, AlgExpEval, AlgExpShow)
    val c = ExpComb.decorator[IEval with IShow](b, (x) => {
      println("visiting " + x)
      x
    })
    println("hello ")
    val exp = b.Add(b.Lit(1), b.Lit(2))
    println(s"${exp.show} = ${exp.eval}")
    val exp1 = c.Add(c.Lit(1), c.Lit(2))
    println(s"${exp1.show} = ${exp1.eval}")
  }
}


class QUM {

  
  def m: AnyRef = {
    class TestTT[T](val b: T) {
    }
    new TestTT[Int](3)
  }
}
