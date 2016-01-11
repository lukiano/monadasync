package monadasync

import algebra.laws._
import cats.Eq
import cats.laws.IsEq
import org.scalacheck.util.{ Pretty, FreqMap }
import org.scalacheck.{ Properties, Prop }
import org.specs2.scalacheck.Parameters
import org.specs2.specification.core.Fragments

trait Laws extends org.typelevel.discipline.Laws {
  implicit def isEqToProp[A: Eq](isEq: IsEq[A]): Prop =
    isEq.lhs ?== isEq.rhs

}
