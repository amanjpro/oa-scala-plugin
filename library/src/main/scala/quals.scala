package ch.usi.inf.l3.oa


import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.reflect.runtime.universe._
import ch.usi.inf.l3.oa.library._



object quals {

  class lifter(x: String, y: String) extends StaticAnnotation
  class merger(x: String) extends StaticAnnotation
  class empty(x: String) extends StaticAnnotation

  def useLifter[T, K](lifter: String): TLifter[T, K] = null
  def useEmpty[T >: Null <: AnyRef](empty: String): T = null
  def mergeWith[T >: Null <: AnyRef](alg: String): T = null
}


