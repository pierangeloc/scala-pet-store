package io.github.pauljamescleary.petstore.free

import cats.{InjectK, ~>}
import cats.data.EitherK
import cats.effect.IO
import cats.free.Free
import io.github.pauljamescleary.petstore.free.OrderRepositoryAlgebra.OrderRepositoryAlgebraF
import io.github.pauljamescleary.petstore.free.PetRepositoryAlgebra.PetRepositoryAlgebraF
import io.github.pauljamescleary.petstore.free.PetValidationAlgebra.{DoesNotExist, PetValidationAlgebraF}
import io.github.pauljamescleary.petstore.model.{Order, Pet}

import scala.collection.concurrent.TrieMap
import scala.util.Random


object services {
  /**
    * Implicit parameter provides a way to say how the `PetRepositoryAlgebraF` can be seen inside a `F`.
    * E.g. a type R can always be seen inside an Either[L, R]. In this case R is a kind
    */
  class PetRepository[F[_]](implicit P: InjectK[PetRepositoryAlgebraF, F]) {
    import PetRepositoryAlgebra._

    type PetRepo[A] = Free[F, A]

    /**
      * Local algebra lives in the wider context of `Repositories` or even of `App`, represented here as `F`
      */
    val liftF = Free.inject[PetRepositoryAlgebraF, F]

    def put(pet: Pet): PetRepo[Pet] = liftF(Put(pet))

    def get(id: Long): PetRepo[Option[Pet]] = liftF(Get(id))

    def delete(id: Long): PetRepo[Option[Pet]] = liftF(Delete(id))

    def findByNameAndCategory(name: String, category: String): PetRepo[Set[Pet]] = liftF(FindByNameAndCategory(name, category))

    def list(pageSize: Int, offset: Int): PetRepo[Seq[Pet]] = liftF(List(pageSize, offset))

  }

  object PetRepository {
    implicit def petRepository[F[_]](implicit P: InjectK[PetRepositoryAlgebraF, F]) = new PetRepository[F]()
  }

  class OrderRepository[F[_]](implicit O: InjectK[OrderRepositoryAlgebraF, F]) {
    import OrderRepositoryAlgebra._

    type OrderRepo[A] = Free[F, A]

    val liftF = Free.inject[OrderRepositoryAlgebraF, F]

    def put(order: Order): OrderRepo[Order] = liftF(Put(order))

    def get(orderId: Long): OrderRepo[Option[Order]] = liftF(Get(orderId))

    def delete(orderId: Long): OrderRepo[Option[Order]] = liftF(Delete(orderId))

  }

  object OrderRepository {
    implicit def orderRepository[F[_]](implicit O: InjectK[OrderRepositoryAlgebraF, F]) = new OrderRepository[F]()
  }

  class PetValidation[F[_]](implicit V: InjectK[PetValidationAlgebraF, F]) {
    type PetValidation[A] = Free[F, A]

    val liftF = Free.inject[PetValidationAlgebraF, F]

    def doesNotExist(pet: Pet): PetValidation[Unit] = liftF(DoesNotExist(pet))

  }

  object PetValidation {
    implicit def petValidation[F[_]](implicit V: InjectK[PetValidationAlgebraF, F]) = new PetValidation[F]()
  }

  type Repositories[A] = EitherK[PetRepositoryAlgebraF, OrderRepositoryAlgebraF, A]
  type PetApp[A] = EitherK[PetValidationAlgebraF, Repositories, A]

  class PetService(implicit P: PetRepository[PetApp], O: OrderRepository[PetApp], V: PetValidation[PetApp]) {
    type R[A] = Free[PetApp, A]


    def create(pet: Pet): R[Pet] = {
      for {
        _ <- V.doesNotExist(pet)
        saved <- P.put(pet)
      } yield saved
    }

    def get(id: Long): R[Pet] = {
      P.get(id).flatMap {
//        case None => E.raiseError(PetNotFoundError(id)) //TODO: Think about it
        case Some(found) => Free.pure(found)
      }
    }

    /* In some circumstances we may care if we actually delete the pet; here we are idempotent and do not care */
    def delete(id: Long): R[Unit] = P.delete(id).map(_ => ())

    def list(pageSize: Int, offset: Int): R[Seq[Pet]] = P.list(pageSize, offset)
  }

  object IOInMemoryInterpreters {

    object InMemoryPetRepositoryInterpreter extends (PetRepositoryAlgebraF ~> IO) {
      import PetRepositoryAlgebra._
      private val cache = new TrieMap[Long, Pet]
      private val random = new Random

      override def apply[A](fa: PetRepositoryAlgebraF[A]): IO[A] = fa match {
        case Put(pet) => IO.pure {
          val toBeInserted = pet.copy(id = pet.id.orElse(Some(random.nextLong)))
          toBeInserted.id.foreach{ cache.put(_, toBeInserted) }
          toBeInserted
        }
        case Get(id) => IO.pure { cache.get(id) }
        case Delete(id) => IO.pure { cache.remove(id) }
        case FindByNameAndCategory(name, category) => IO.pure {
          cache.values.filter{ p => p.name == name && p.category == category}.toSet
        }
        case List(pageSize, offset) => IO.pure {
          cache.values.toSeq.slice(offset, offset + pageSize)
        }
      }
    }

    object InMemoryOrderRepositoryInterpreter extends (OrderRepositoryAlgebraF ~> IO) {
      import OrderRepositoryAlgebra._

      private val cache = new TrieMap[Long, Order]
      private val random = new Random

      override def apply[A](fa: OrderRepositoryAlgebraF[A]): IO[A] = fa match {
        case Put(order) => IO.pure {
          val toBeInserted = order.copy(id = order.id.orElse(Some(random.nextLong)))
          toBeInserted.id.foreach{ cache.put(_, toBeInserted) }
          toBeInserted
        }
        case Get(orderId) => IO.pure { cache.get(orderId) }
        case Delete(orderId) => IO.pure { cache.remove(orderId) }
      }
    }

    object PetValidationInterpreter extends (PetValidationAlgebraF ~> IO) {
      import PetValidationAlgebra._

      override def apply[A](fa: PetValidationAlgebraF[A]): IO[A] = fa match {
        case DoesNotExist(pet: Pet) => IO.pure(())
      }
    }

    val repositoriesInterpreter: Repositories ~> IO = InMemoryPetRepositoryInterpreter or InMemoryOrderRepositoryInterpreter
    val interpreter: PetApp ~> IO = PetValidationInterpreter or repositoriesInterpreter

    val runnablePetService = new PetService() {
      def run[A](program: R[A]): IO[A] = program.foldMap(interpreter)
    }
  }

}
