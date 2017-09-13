package io.github.pauljamescleary.petstore.free

import io.github.pauljamescleary.petstore.model.Order

object OrderRepositoryAlgebra {

  sealed trait OrderRepositoryAlgebraF[A]

  case class Put(order: Order) extends OrderRepositoryAlgebraF[Order]
  case class Get(orderId: Long) extends OrderRepositoryAlgebraF[Option[Order]]
  case class Delete(orderId: Long) extends OrderRepositoryAlgebraF[Option[Order]]
}