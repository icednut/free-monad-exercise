package io.icednut.exercise

import cats.effect.{IO, IOApp}
import cats.{Monad, ~>}
import io.icednut.exercise.FreeMonad.Free.pure

import scala.collection.mutable

object FreeMonad extends IOApp.Simple {

  // pure(a) => M[A]
  // flatMap(a => M[B]) => M[B]

//  trait Monad[M[_]] {
//    def pure[A](a: A): M[A]
//    def flatMap[A, B](a: M[A])(f: A => M[B]): M[B]
//  }

  // this natural transformation will be a transformation between the original M data type which in my original program was DBOps
  // and the new G data type which is in my case IO thing that I'ec just defined a couple of seconds ago.
//  trait ~>[F[_], G[_]] {
//    def apply[A](fa: F[A]): G[A]
//  }

  trait Free[M[_], A] {
    import Free.*
    def flatMap[B](f: A => Free[M, B]): Free[M, B] = FlatMap(this, f)
    def map[B](f: A => B): Free[M, B] = flatMap((a: A) => pure(f(a)))
    def foldMap[G[_]: Monad](natTrans: M ~> G): G[A] = this match {
      case Pure(a) =>
        Monad[G].pure(a)
      case FlatMap(fa, f) =>
        Monad[G].flatMap(fa.foldMap(natTrans))(a => f(a).foldMap(natTrans))
      case Suspend(ma) =>
        natTrans.apply[A](ma)
    }
  }

  object Free {
    def pure[M[_], A](a: A): Free[M, A] = Pure(a)
    def liftM[M[_], A](ma: M[A]): Free[M, A] = Suspend(ma)

    case class Pure[M[_], A](a: A) extends Free[M, A]
    case class FlatMap[M[_], A, B](fa: Free[M, A], f: A => Free[M, B]) extends Free[M, B]
    case class Suspend[M[_], A](ma: M[A]) extends Free[M, A]
  }

  // why we need free monad?
  // 1. we can use free monad to implement interpreter
  // 2. we can use free monad to implement test
  // 3. we can use free monad to implement DSL
  // 4. we can use free monad to implement effect system
  // 5. we can use free monad to implement coroutine
  // 6. we can use free monad to implement async
  // 7. we can use free monad to implement state machine
  // 8. we can use free monad to implement parser
  // 9. we can use free monad to implement interpreter
  // sequence computations as data structures, THEN attach the monadic type at the end
  // this is "algebra"
  trait DBOps[A]
  case class Create[A](key: String, value: A) extends DBOps[Unit]
  case class Read[A](key: String) extends DBOps[A]
  case class Update[A](key: String, value: A) extends DBOps[A]
  case class Delete(key: String) extends DBOps[Unit]

  // definition - fancier algebra
  type DBMonad[A] = Free[DBOps, A]

  // "smart" constructors
  def create[A](key: String, value: A): DBMonad[Unit] =
    Free.liftM[DBOps, Unit](Create(key, value))

  def get[A](key: String): DBMonad[A] =
    Free.liftM[DBOps, A](Read[A](key))

  def update[A](key: String, value: A): DBMonad[A] =
    Free.liftM[DBOps, A](Update[A](key, value))

  def delete(key: String): DBMonad[Unit] =
    Free.liftM(Delete(key))

  // we're now using the monadic structure of free to be able to sequence all these computations together
  // and a full program that writes or reads things from the fictious database

  // business logic is FIXED
  // but rather the evaluation/interpretation of this business logic can vary or can be changed later
  // so the little program is very important and needs to be fixed but the evaluation of that can be changed
  // this is why the free monad is such a useful tool and this can only be performed under the presence of the foldMap method of the free monad
  // so if we can perform or create a natural transformation between DBOps the abstract algebra(Create, Read, Update, Delete) into actual effects that
  // will perform something in real life then we can evaluate the little program under foldMap
  def myLittleProgram: DBMonad[Unit] = for {
    _ <- create[String]("123-456", "Will")
    name <- get[String]("123-456")
    _ <- create[String]("567", name.toUpperCase())
    _ <- delete("123-456")
  } yield () // this is description of a computation

  // evaluate the program -> we need some sort of interpreter/"compiler"
  // a good candidate for that IO data type from Cats Effect/ZIO

  // given by the structure of the free monad we can replace io by any other kind of structure

  // and now to that end we'll need a function in the free monad that's called foldMap.


  import scala.collection.mutable
  val myDB: mutable.Map[String, String] = mutable.Map[String, String]()
  // TODO replace these with some real serialization
  def serialize[A](a: A): String = a.toString
  def deserialize[A](value: String): A = {
    value.asInstanceOf[A]
  }

  // so let's perform a natural transformation between DBOps and IO
  val dbOps2IO: DBOps ~> IO = new (DBOps ~> IO) {
    override def apply[A](fa: DBOps[A]): IO[A] = fa match {
      case Create(key, value) => IO {
        // acutal code that uses the database
        println(s"insert into people(id, name) values($key, $value)")
        myDB += (key -> serialize(value))
        ()
      }
      case Read(key) => IO {
        println(s"select * from people where id=$key limit 1")
        deserialize[A](myDB(key))
      }
      case Update(key, value) => IO {
        println(s"update people(name=$value) where id=$key")
        val oldValue = myDB(key)
        myDB += (key -> serialize(value))
        deserialize[A](oldValue)
      }
      case Delete(key) => IO {
        println(s"delete from people where id=$key")
        ()
      }
    }
  }

  val ioProgram: IO[Unit] = myLittleProgram.foldMap(dbOps2IO)


//  def main(args: Array[String]): Unit = {
//  }

  override def run: IO[Unit] = ioProgram
}
