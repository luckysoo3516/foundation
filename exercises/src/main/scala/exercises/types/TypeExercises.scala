package exercises.types

import java.time.Instant
import java.util.{Date, UUID}

import cats.data.NonEmptyList
import eu.timepit.refined.types.numeric.PosInt
import exercises.sideeffect.IOExercises.IO
import exercises.types.Card._
import exercises.types.TypeExercises.OrderStatus.{Canceled, CheckOut, Delivered, Submitted}

// You can run and print things here:
object TypeApp extends App {
  import TypeExercises._

  println(boolean.cardinality)
}

object TypeExercises {

  ////////////////////////
  // 1. Misused types
  ////////////////////////

  // 1a. Implement `compareChar` that indicates if `c1` is smaller, equal to or larger than `c2`
  // such as compareChar('a', 'c') == -1
  //         compareChar('c', 'c') ==  0
  //         compareChar('c', 'a') ==  1
  // What is wrong with this function? How could you improve it?
  sealed trait CompareResult
  object CompareResult {
    case object Greater extends CompareResult
    case object Equal extends CompareResult
    case object Less extends CompareResult
  }

  def compareChar(c1: Char, c2: Char): CompareResult =
    if(c1 > c2) CompareResult.Greater
    else if(c1 == c2) CompareResult.Equal
    else CompareResult.Less

  // 1b. Implement `mostRecentBlogs` that returns the `n` most recent blog posts
  // such as mostRecentBlogs(1)(List(
  //   BlogPost(1,First blog,2019-09-18T16:21:06.681768Z)
  //   BlogPost(23,Thoughts of the day,2019-09-21T08:14:06.702836Z)
  // )) == List(BlogPost(23,Thoughts of the day,2019-09-21T08:14:06.702836Z))
  // What is wrong with this function? How could you improve it?
  case class BlogPost(id: Int, title: String, createAt: Date)

  def mostRecentBlogs(n: Int)(blogs: List[BlogPost]): List[BlogPost] =
    blogs.sortBy(_.createAt).take(n)

  // 1c. Implement `User#address` that returns the full address for a User (e.g. to send a parcel)
  // such as User("John Doe", Some(108), Some("Cannon Street"), Some("EC4N 6EU")) == "108 Canon Street EC4N 6EU"
  // What is wrong with this function? How could you improve it?
  case class User(name: String, streetNumber: Option[Int], streetName: Option[String], postCode: Option[String]) {
    def address: Option[String] =
      for {
        snumber <- streetNumber
        sname <- streetName
        pcode <- postCode
      } yield s"$snumber $sname $pcode"
  }

  // 1d. Implement `Invoice#discountFirstItem` that returns a new invoice with the first item discounted.
  // For example, discountFirstItem(0.3) would apply a 30% discount.
  // What is wrong with this function? How could you improve it?
  case class InvoiceItem(id: String, quantity: Int, price: Double)
  // An invoice must have at least one item.
  case class Invoice(id: String, items: List[InvoiceItem]) {
    def discountFirstItem(discountPercent: Double): Either[Throwable, Invoice] =
      if(items.head.quantity == 0) Left(new RuntimeException("An invoice must have at least one item."))
      else {
        val newPrice = items.head.price * (1- discountPercent)
        val newHead = items.head.copy(price = newPrice)
        val newTail = items.tail
        Right(copy(items = newHead :: newTail))
      }
  }

  // 1e. Implement `createTicket` that instantiates a Ticket with 0 story point,
  // a random ticket id (see `genTicketId`) and the current time (see `readNow`).
  // What is wrong with this function? How could you improve it?
  def createTicket(title: String): IO[Ticket] = {
    for {
      id <- genTicketId
      createdAt <- readNow
    } yield Ticket(id, title, 0, createdAt)
  }

  def genTicketId: IO[TicketId] = IO.effect(TicketId(UUID.randomUUID()))
  def readNow: IO[Instant]      = IO.effect(Instant.now())

  case class TicketId(value: UUID)
  case class Ticket(id: TicketId, title: String, storyPoints: Int, createdAt: Instant)

  ////////////////////////
  // 2. Data Encoding
  ////////////////////////

  // 2a. Create types that encode the following business requirements:
  // An order contains an order id (UUID), a created timestamp (Instant), an order status, and a basket of items.
  // An order status is either a draft, checkout, submitted or delivered.
  // An item consists of an item id (UUID), a quantity and a price.
  // A basket can be empty in draft, otherwise it must contain at least one item.
  // When an order is in checkout, it may have a delivery address.
  // When an order is in submitted, it must have a delivery address and a submitted timestamp (Instant).
  // When an order is in delivered, it must have a delivery address, a submitted and delivered timestamps (Instant).
  // An address consists of a street number and a post code.
  case class Order(id:UUID, createdAt: Instant, status: OrderStatus)
  sealed trait OrderStatus
  object OrderStatus {
    case class Draft(basket: List[Item]) extends OrderStatus
    case class CheckOut(address: Option[Address], basket: NonEmptyList[Item]) extends OrderStatus
    case class Submitted(address: Address, submittedAt: Instant, basket: NonEmptyList[Item]) extends OrderStatus
    case class Delivered(address: Address, submittedAt: Instant, deliveredAt: Instant, basket: NonEmptyList[Item]) extends OrderStatus
    case class Canceled(status: Either[CheckOut, Submitted], canceledAt: Instant) extends OrderStatus
  }
  case class Item(id:UUID, quantity: Int, price: Double)
  case class Address(streetNumber:Int, postCode: String)
  // 2b. Implement `submit` which encodes the order transition between `Checkout` to `Submitted`.
  // Verify all pre and post conditions are satisfied and if not encode the errors in an ADT.
  // What parameters should submit take?

  sealed trait SubmitError
  object SubmitError {
    case object NoAddressError extends SubmitError
    case object NoCheckOutError extends SubmitError
  }
  def submit(order: Order): Either[SubmitError, Order] =
    order.status match {
      case CheckOut(None, _) =>
        Left(SubmitError.NoAddressError)
      case CheckOut(Some(address), basket: NonEmptyList[Item]) =>
        Right(order.copy(status = Submitted(address, Instant.now(), basket)))
      case _ =>
        Left(SubmitError.NoCheckOutError)
    }

  // 2c. Implement `deliver` which encodes the order transition between `Submitted` to `Delivered` status.
  // Verify all pre and post conditions are satisfied and, if not, encode the errors in an ADT.
  // You may need to modify your encoding to eliminate runtime errors.
//  case class Draft(basket: List[Item]) extends OrderStatus
//  case class CheckOut(address: Option[Address], basket: NonEmptyList[Item]) extends OrderStatus
//  case class Submitted(address: Address, submittedAt: Instant, basket: NonEmptyList[Item]) extends OrderStatus
//  case class Delivered(address: Address, submittedAt: Instant, deliveredAt: Instant, basket: NonEmptyList[Item]) extends OrderStatus
  sealed trait DeliverError
  object DeliverError {
    case object NoSubmitted extends DeliverError
  }

  def deliver(order: Order): Either[DeliverError, Order] =
    order.status match {
      case Submitted(address, submittedAt, basket) =>
        Right(order.copy(status = Delivered(address, submittedAt, Instant.now(), basket)))
      case _ =>
        Left(DeliverError.NoSubmitted)
    }

  // 2d. Add a cancelled status.
  // An order can be cancelled only if it has a `Checkout` or `Submitted` status.
  // A cancelled order must have a cancelled timestamp (Instant).


  // 2e. Implement `cancel` which encodes the order transition between `Checkout` or `Submitted` to `Cancelled` status.
  // Verify all pre and post conditions are satisfied and, if not, encode the errors in an ADT.
  // You may need to modify your encoding to eliminate runtime errors.
  sealed trait CancelError
  object CancelError {
    case object NeitherCheckoutNorSubmitted extends CancelError
  }

  def cancel(order:Order): Either[CancelError, Order] =
    order.status match {
      case checkout: CheckOut => Right(order.copy(status = Canceled(Left(checkout), Instant.now())))
      case submitted: Submitted => Right(order.copy(status = Canceled(Right(submitted), Instant.now())))
      case _ => Left(CancelError.NeitherCheckoutNorSubmitted)
    }

  ////////////////////////
  // 3. Cardinality
  ////////////////////////

  val boolean: Cardinality[Boolean] = new Cardinality[Boolean] {
    def cardinality: Card = Constant(2)
  }

  val int: Cardinality[Int] = new Cardinality[Int] {
    def cardinality: Card = Constant(2) ^ Constant(32)
  }

  // 3a. How many possible values exist of type Any?
  val any: Cardinality[Any] = new Cardinality[Any] {
    def cardinality: Card = Inf
  }

  // 3b. How many possible values exist of type Nothing?
  val nothing: Cardinality[Nothing] = new Cardinality[Nothing] {
    def cardinality: Card = Constant(0)
  }

  // 3c. How many possible values exist of type Unit?
  val unit: Cardinality[Unit] = new Cardinality[Unit] {
    def cardinality: Card = ???
  }

  // 3d. How many possible values exist of type Byte?
  val ioUnit: Cardinality[IO[Unit]] = new Cardinality[IO[Unit]] {
    def cardinality: Card = ???
  }

  // 3e. How many possible values exist of type Option[Boolean]?
  val optBoolean: Cardinality[Option[Boolean]] = new Cardinality[Option[Boolean]] {
    def cardinality: Card = ???
  }

  // 3f. How many possible values exist of type IntOrBoolean?
  val intOrBoolean: Cardinality[IntOrBoolean] = new Cardinality[IntOrBoolean] {
    def cardinality: Card = ???
  }

  sealed trait IntOrBoolean
  object IntOrBoolean {
    case class AnInt(value: Int)        extends IntOrBoolean
    case class ABoolean(value: Boolean) extends IntOrBoolean
  }

  // 3g. How many possible values exist of type IntAndBoolean?
  val intAndBoolean: Cardinality[IntAndBoolean] = new Cardinality[IntAndBoolean] {
    def cardinality: Card = ???
  }

  case class IntAndBoolean(i: Int, b: Boolean)

  ///////////////////////
  // GO BACK TO SLIDES
  ///////////////////////

  // 3h. How many possible values exist of type Option[Nothing]?
  val optNothing: Cardinality[Option[Nothing]] = new Cardinality[Option[Nothing]] {
    def cardinality: Card = ???
  }

  // 3i. How many possible values exist of type (Boolean, Nothing)?
  val boolNothing: Cardinality[(Boolean, Nothing)] = new Cardinality[(Boolean, Nothing)] {
    def cardinality: Card = ???
  }

  // 3j. How many possible implementation exist for `getCurrency`?
  def getCurrency: Cardinality[Country => Currency] = new Cardinality[Country => Currency] {
    def cardinality: Card = ???
  }

  // 3k. How many possible implementation exist for `getCurrencyString`? Is it more or less than `getCurrency`?
  def getCurrencyString: Cardinality[String => Option[String]] = new Cardinality[String => Option[String]] {
    def cardinality: Card = ???
  }

  // 3l. How many possible values exist of type A => B?
  def func[A, B](a: Cardinality[A], b: Cardinality[B]): Cardinality[A => B] =
    new Cardinality[A => B] {
      def cardinality: Card = ???
    }

  // 3m. Can you think of a function signature with only one implementation?
  // i.e. find A1, A2 such as |A1 => A2| = 1.

  // 3n. Can you provide an example of a function signature with no implementation?
  // i.e. find A1, A2 such as |A1 => A2| = 0.

  ////////////////////////
  // 4. Parametricity
  ////////////////////////

  // 4a. How many implementations exist for `id` (assume we are using functional subset)?
  def id[A](a: A): A = ???

  // 4b. How many implementations exist for `mapOption`?
  def mapOption[A, B](opt: Option[A])(f: A => B): Option[B] = ???  //(B+1)^((A + 1) * (B^A))

  // 4c. How many implementations exist for `mapOptionIntToBool`?
  def mapOptionIntToBool(opt: Option[Int])(f: Int => Boolean): Option[Boolean] = ??? // 3 * (|Int| + 1)*(3^|Int+1|)

  // 4d. How would you test `mapOption` to be sure there is no bug?
  // 모든 경우의 수를 테스트해본다.
  // 4e. How many implementations exist for `mapList`? How would you test it?
  def mapList[A, B](xs: List[A])(f: A => B): List[B] = (|B|)^((|A|) * (|B|^|A|))

  ////////////////////////
  // 5. Tests
  ////////////////////////

  // 5a. Given `getCurrency` signature, what is the VIC of of `getCurrency`
  // if we have one unit test, e.g. assert(getCurrency(France) == EUR)?
  // If we have two unit tests, e.g. assert(getCurrency(France) == EUR) and assert(getCurrency(Germany) = EUR)?
  def getCurrency(country: Country): Currency = ??? // VIC(getCurrency) = 2^3 - 1 or 2^3 - 2

  sealed trait Country
  object Country {
    case object France        extends Country
    case object Germany       extends Country
    case object UnitedKingdom extends Country
  }

  sealed trait Currency
  object Currency {
    case object EUR extends Currency
    case object GBP extends Currency
  }

  // 5b. Given `sign` signature, what is the VIC of of `sign`
  // if we have one unit test, e.g. assert(sign(-2) == false)?
  // If we have two unit tests, e.g. assert(sign(-2) == false), assert(sign(0) == true) and assert(sign(5) == true) ?
  def sign(x: Int): Boolean = ??? // 2 ^ |INT| - 1 or 2 ^ |INT| - 2

  // 5c. Can you define the VIC formula for any function A => B with n different unit tests?
   // VIC(A=>B) = B^A - n

  // 5d. What is the VIC of `sign` if it has the following property based test:
  // forAll(x: Int => sign(x) == !sign(-x)). // VIC(sign) = 0

  // 5e. Can you define the VIC formula for any function A => B with n different property based tests?
    // VIC(A=>B) = 0
  ////////////////////////
  // 6. Type Algebra
  ////////////////////////

  // 6a. In basic algebra, a * 1 = 1 * a = a and a + 0 = 0 + a = a (we say that 1 is the unit of * and 0 is the unit of +).
  // Is it also true with types?
  // To prove that two types A and B are equivalent you need to provide a pair of functions `to` and `from`
  // such as for all a: A, from(to(a)) == a, and equivalent for B.
  def from[A](x: (A, Unit)): A
  def to[A](x: A): (A, Unit)

  def aUnitToA[A]: Iso[(A, Unit), A] =
    Iso[(A, Unit), A](
      { a: (A, Unit) => a._1 },
      { a: A => (a, Unit)}
    )

  def aOrNothingToA[A]: Iso[Either[A, Nothing], A] =
    Iso(
      {
        case Left(x) => x
      },
      {  Left(_) }
    )

  // 6b. Prove that `Option[A]` is equivalent to `Either[Unit, A]`.
  def optionToEitherUnit[A]: Iso[Option[A], Either[Unit, A]] =
    Iso(
      {
        case Some(x) => Right(x)
      },
      { case Right(x) => Some(x)}
    )

  // 6c. Prove that a * (b + c) = a * b + a * c.
  def distributeTuple[A, B, C]: Iso[(A, Either[B, C]), Either[(A, B), (A, C)]] =
    Iso(
      {
        case (a, Left(b)) => Left((a, b)) // Either[(A, B), (A, C)]]
        case (a, Right(c)) => Right((a, c)) // Either[(A, B), (A, C)]]
      }
      {
        case Left((a, b)) => (a, Left(b))
        case Right((a,c)) => (a, Right(c))
      }
    )

  // 6d. Prove that a ^ 1 = a.
  def power1[A]: Iso[Unit => A, A] =
    new Iso[Unit => A, A](
      {a: (Unit => A) => a(())},
      {a: A => _ => a}
    )

  // 6e. Can you think of any other properties that types and algebra have in common?

}
