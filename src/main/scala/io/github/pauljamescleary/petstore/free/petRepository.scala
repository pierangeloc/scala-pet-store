package io.github.pauljamescleary.petstore.free

import cats.effect.IO
import cats.free.Free
import cats.~>
import io.github.pauljamescleary.petstore.free.PetRepositoryAlgebra.PetRepositoryAlgebraF
import io.github.pauljamescleary.petstore.model.Pet

object PetRepositoryAlgebra {

  sealed trait PetRepositoryAlgebraF[A]

  case class Put(pet: Pet) extends PetRepositoryAlgebraF[Pet]
  case class Get(id: Long) extends PetRepositoryAlgebraF[Option[Pet]]
  case class Delete(id: Long) extends PetRepositoryAlgebraF[Option[Pet]]
  case class FindByNameAndCategory(name: String, category: String) extends PetRepositoryAlgebraF[Set[Pet]]
  case class List(pageSize: Int, offset: Int) extends PetRepositoryAlgebraF[Seq[Pet]]

}

trait PetRepository {
  import PetRepositoryAlgebra._
  type PetRepo[A] = Free[PetRepositoryAlgebraF, A]


  def put(pet: Pet): PetRepo[Pet] = Free.liftF(Put(pet))

  def get(id: Long): PetRepo[Option[Pet]] = Free.liftF(Get(id))

  def delete(id: Long): PetRepo[Option[Pet]] = Free.liftF(Delete(id))

  def findByNameAndCategory(name: String, category: String): PetRepo[Set[Pet]] = Free.liftF(FindByNameAndCategory(name, category))

  def list(pageSize: Int, offset: Int): PetRepo[Seq[Pet]] = Free.liftF(List(pageSize, offset))

}

trait PetRepositoryIOInterpreter extends PetRepository {
  def natTrans: PetRepositoryAlgebraF ~> IO

  def apply[A](program: PetRepo[A]): IO[A] = program.foldMap(natTrans)
}
