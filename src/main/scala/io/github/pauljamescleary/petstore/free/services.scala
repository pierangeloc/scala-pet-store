package io.github.pauljamescleary.petstore.free

import cats.InjectK
import cats.data.EitherK
import cats.free.Free
import io.github.pauljamescleary.petstore.free.OrderRepositoryAlgebra.{Delete, Get, OrderRepositoryAlgebraF, Put}
import io.github.pauljamescleary.petstore.free.PetRepositoryAlgebra.{Delete, FindByNameAndCategory, Get, List, PetRepositoryAlgebraF, Put}
import io.github.pauljamescleary.petstore.free.PetValidationAlgebra.{DoesNotExist, PetValidationAlgebraF}
import io.github.pauljamescleary.petstore.model.{Order, Pet}


object services {

  type Repositories[A] = EitherK[PetRepositoryAlgebraF, OrderRepositoryAlgebraF, A]
  type App[A] = EitherK[PetValidationAlgebraF, Repositories, A]

  /**
    * Implicit parameter provides a way to say how the `PetRepositoryAlgebraF` can be derived from the `F`
    * @param P
    * @tparam F
    */
  class PetRepository[F[_]](implicit P: InjectK[PetRepositoryAlgebraF, F]) {
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

  class PetService(implicit P: PetRepository[App], O: OrderRepository[App], V: PetValidation[App]) {
    type R[A] = Free[App, A]


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

}
