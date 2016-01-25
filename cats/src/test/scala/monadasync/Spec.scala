package monadasync

import org.scalacheck.Arbitrary
import org.scalatest.{ AsyncFunSuite, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.prop.Checkers

abstract class Spec extends AsyncFunSuite with Matchers with GeneratorDrivenPropertyChecks with Checkers {

  def checkAll(name: String, ruleSet: Laws#RuleSet) {
    for ((id, prop) ← ruleSet.all.properties)
      test(name + "." + id) {
        check(prop)
      }
  }

  implicit def arbitraryF0(implicit a: Arbitrary[Int]): Arbitrary[() => Int] = Arbitrary {
    a.arbitrary map { a => () => a }
  }
}
