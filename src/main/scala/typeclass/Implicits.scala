package typeclass


object Implicits {

}

object Task1 {

  final case class Money(amount: BigDecimal)

  // TODO: create Ordering instance for Money
  implicit val moneyOrdering: Ordering[Money] = (x: Money, y: Money) => x.amount compare y.amount
}

object Task2 {

  trait Show[T] { // fancy toString
    def show(entity: T): String
  }

  object Show {
    def apply[F](implicit instance: Show[F]): Show[F] = instance
  }

  final case class User(id: String, name: String)

  implicit val userShow: Show[User] = (user: User) => s"User №${user.id} is named like ${user.name}"

  object ShowSyntax {

    implicit class ShowOps[A](x: A) {
      def show(implicit s: Show[A]): String = {
        s.show(x)
      }
    }

  }
  // TODO: create Show instance for User
  // TODO: create syntax for Show so i can do User("1", "Oleg").show
}

object Task3 {
  type Error = String

  trait Parse[T] { // invent any format you want or it can be csv string
    def parse(entity: String): Either[Error, T]
  }

  final case class User(id: String, name: String)

  implicit val parseUser: Parse[User] = (entity: String) => {
    val supposedlyUser = entity.split(" ").toList
    if (supposedlyUser.length <= 1) {
      Left("Error: not a user")
    }
    else {
      Right(User(supposedlyUser.head, supposedlyUser.tail.head))
    }
  }

  object ParseSyntax {

    implicit class ParseOps[A](x: String) {
      def parse(implicit p: Parse[A]): Either[Error, A] = {
        p.parse(x)
      }
    }

  }
  // TODO: create Parse instance for User
  // TODO: create syntax for Parse so i can do "lalala".parse[User] (and get an error because it is obviously not a User)
}

object Task4 {
  // just a try, not sure at all
  trait KindaEqual[A, B] {
    def ===(a: A)(b: B): Boolean
  }
  // TODO: design a typesafe equals so i can do a === b, but it won't compile if a and b are of different types
  // define the typeclass (think of a method signature)
  // remember `a method b` is `a.method(b)`
}

object AdvancedHomework {

  trait Functor[F[_]] {
    def flatMap[A, B](x: F[A])(f: A => Iterable[B]): F[B]
  }
  // TODO: create a typeclass for flatMap method
}

object TypeclassTask {

  // Why am I not a Typeclass?
  // TODO: Rework me so I am a typeclass
  trait HashCode[T] {
    def hash(entity: T): Int
  }

  object HashCode {
    def apply[F](implicit instance: HashCode[F]): HashCode[F] = instance
    // TODO: Implement me a summoner
  }

  implicit val stringHashCode: HashCode[String] = (entity: String) => entity.hashCode

  implicit class HashCodeSyntax[A](x: A) {
    def hash(implicit i: HashCode[A]): Int = {
      i.hash(x)
    }
    // TODO: Implement syntax so I can "abc".hash
  }
  // TODO: make an instance for String
  // TODO: write "abc".hash to check everything
}
