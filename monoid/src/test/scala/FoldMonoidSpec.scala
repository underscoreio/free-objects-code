import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object FoldMonoidSpecification extends Properties("FoldMonoid") {
  import scalaz.std.anyVal._

  property("apply folds monoid over list") = forAll { (a: List[Int]) =>
    FoldMonoid(a) == a.sum
  }
}
