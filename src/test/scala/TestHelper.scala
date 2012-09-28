import org.scalatest.matchers.ShouldMatchers

/**
 * By inheriting from this trait, you could use the should_equal matcher.
 * This basically works like the "should equal" matcher from Scalatest, but checks both directions (a==b and b==a).
 * It also checks that the hashcode is equal. 
 * 
 * Examples:
 * 
 *   a should_equal b      //Checks for a==b, b==a and a.hashCode==b.hashCode
 *   a shuold_not_equal b  //Checks for a!=b and b!=a
 */
trait TestHelper extends ShouldMatchers {
  
  //Allow the should_equal matcher
  //This matcher extends the should equal matchers from ScalaTest by checking
  //lhs.equals(rhs) && rhs.equals(lhs) && lhs.hashCode==rhs.hashCode
  implicit def any2ShouldEqual[T](lhs: T) = new {
    def should_equal[U](rhs: U) {
      lhs should equal (rhs)
      rhs should equal (lhs)
      lhs.hashCode should equal (rhs.hashCode)
    }
    def should_not_equal[U](rhs: U) {
      lhs should not equal (rhs)
      rhs should not equal (lhs)
    }
  }
}