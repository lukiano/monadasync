package monadasync

import org.scalacheck.{ Arbitrary, Properties }
import org.scalacheck.util.{ Pretty, FreqMap }
import org.specs2.ScalaCheck
import org.specs2.main.{ ArgumentsArgs, ArgumentsShortcuts }
import org.specs2.matcher.{ StandardMatchResults, MatchersImplicits }
import org.specs2.mutable.SpecLike
import org.specs2.scalacheck.Parameters
import org.specs2.specification.core.Fragments

abstract class Spec extends SpecLike with ScalaCheck
    with MatchersImplicits with StandardMatchResults
    with ArgumentsShortcuts with ArgumentsArgs {

  val ff = fragmentFactory; import ff._

  def checkAll(name: String, ruleSet: Laws#RuleSet): Unit =
    checkAll(name, ruleSet.all)

  private def checkAll(name: String, props: Properties)(implicit p: Parameters, f: FreqMap[Set[Any]] => Pretty): Unit = {
    addFragment(text(s"$name ${props.name} must satisfy"))
    addFragment(break)
    addFragments(Fragments.foreach(props.properties) { case (fragName, prop) => Fragments(fragName in check(prop, p, f)) })
    ()
  }

  implicit def arbitraryF0(implicit a: Arbitrary[Int]): Arbitrary[() => Int] = Arbitrary {
    a.arbitrary map { a => () => a }
  }
}
