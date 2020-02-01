package errorhandling

import java.time.{Duration, Instant, LocalDate, ZoneOffset}
import java.util.UUID

import exercises.errorhandling.EitherExercises.CountryError.InvalidFormat
import exercises.errorhandling.EitherExercises.UserEmailError.{EmailNotFound, UserNotFound}
import exercises.errorhandling.EitherExercises.UsernameError.{InvalidCharacters, TooSmall}
import exercises.errorhandling.EitherExercises._
import exercises.errorhandling.OptionExercises
import org.scalatest.Matchers
import org.scalatest.funsuite.AnyFunSuite

class EitherExercisesTest extends AnyFunSuite with Matchers {

  def startOfDay(year: Int, month: Int, day: Int): Instant =
    LocalDate.of(year, month, day).atStartOfDay.toInstant(ZoneOffset.UTC)

  ////////////////////////
  // 1. Use cases
  ////////////////////////

  test("getUserEmail") {
    import OptionExercises.{Email, User, UserId}
    val userMap = Map(
      UserId(222) -> User(UserId(222), "john", Some(Email("j@x.com"))),
      UserId(123) -> User(UserId(123), "elisa", Some(Email("e@y.com"))),
      UserId(444) -> User(UserId(444), "bob", None)
    )

    getUserEmail(UserId(123), userMap) shouldEqual Right(Email("e@y.com"))
    getUserEmail(UserId(111), userMap) shouldEqual Left(UserNotFound(UserId(111)))
    getUserEmail(UserId(444), userMap) shouldEqual Left(EmailNotFound(UserId(444)))
  }

  test("checkout") {
    val item      = Item("xxx", 2, 12.34)
    val baseOrder = Order("123", "Draft", List(item), None, None, None)

    checkout(baseOrder) shouldEqual Right(baseOrder.copy(status = "Checkout"))
  }

  test("submit") {
    val item      = Item("xxx", 2, 12.34)
    val now       = startOfDay(2019, 6, 12)
    val baseOrder = Order("123", "Checkout", List(item), Some("10 high street"), None, None)

    submit(baseOrder, now) shouldEqual Right(baseOrder.copy(status = "Submitted", submittedAt = Some(now)))
    submit(baseOrder.copy(deliveryAddress = None), now) shouldEqual Left("Checkout")
    submit(baseOrder.copy(status = "Draft"), now) shouldEqual Left("Draft")
  }
  test("deliver") {
    val item        = Item("xxx", 2, 12.34)
    val submittedAt = startOfDay(2019, 6, 12)
    val now         = startOfDay(2019, 6, 15)
    val baseOrder   = Order("123", "Submitted", List(item), Some("10 high street"), Some(submittedAt), None)

    deliver(baseOrder, now) shouldEqual Right(
      (baseOrder.copy(status = "Delivered", deliveredAt = Some(now)), Duration.ofDays(3))
    )
    deliver(baseOrder.copy(submittedAt = None), now) shouldEqual Left("Submitted")
    deliver(baseOrder.copy(status = "Draft"), now) shouldEqual Left("Draft")

  }

  //////////////////////////////////
  // 2. Import code with Exception
  //////////////////////////////////

  test("parseUUID") {

    parseUUID("123e4567-e89b-12d3-a456-426655440000") shouldEqual Right(
      UUID.fromString("123e4567-e89b-12d3-a456-426655440000")
    )
    parseUUID("foo").isLeft shouldEqual true
  }

  //////////////////////////////////
  // 3. Advanced API
  //////////////////////////////////

  test("validateUsername") {
    validateUsername("foo") shouldEqual Right(Username("foo"))
    validateUsername("  foo ") shouldEqual Right(Username("foo"))
    validateUsername("a!bc@£") shouldEqual Left(InvalidCharacters("!@£".toList))
    validateUsername(" yo") shouldEqual Left(TooSmall(2))
    validateUsername(" !") shouldEqual Left(TooSmall(1))
  }

  test("validateUsernameSize") {
    validateUsernameSize("moreThan3Char") shouldEqual Right(())
    validateUsernameSize("foo") shouldEqual Right(())
    validateUsernameSize("fo") shouldEqual Left(TooSmall(2))
  }

  test("validateUsernameCharacters") {
    validateUsernameCharacters("abcABC123-_") shouldEqual Right(())
    validateUsernameCharacters("foo!~23}AD") shouldEqual Left(InvalidCharacters(List('!', '~', '}')))
  }

  test("validateUser") {}

  test("validateUserAcc") {}

  test("sequenceAcc") {}

}
