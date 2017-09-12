package io.github.pauljamescleary.petstore.free

import cats.effect.IO
import cats.free.Free
import cats.~>
import io.github.pauljamescleary.petstore.free.OrderRepositoryAlgebra.OrderRepositoryAlgebraF
import io.github.pauljamescleary.petstore.model.Order

object OrderRepositoryAlgebra {

  sealed trait OrderRepositoryAlgebraF[A]

  case class Put(order: Order) extends OrderRepositoryAlgebraF[Order]
  case class Get(orderId: Long) extends OrderRepositoryAlgebraF[Option[Order]]
  case class Delete(orderId: Long) extends OrderRepositoryAlgebraF[Option[Order]]
}

trait OrderRepository {
  import OrderRepositoryAlgebra._
  type OrderRepo[A] = Free[OrderRepositoryAlgebraF, A]

  def put(order: Order): OrderRepo[Order] = Free.liftF(Put(order))

  def get(orderId: Long): OrderRepo[Option[Order]] = Free.liftF(Get(orderId))

  def delete(orderId: Long): OrderRepo[Option[Order]] = Free.liftF(Delete(orderId))

}

trait OrderRepositoryIOInterpreter extends OrderRepository{

  def natTrans: OrderRepositoryAlgebraF ~> IO

  def apply[A](program: OrderRepo[A]): IO[A] = program.foldMap(natTrans)
}