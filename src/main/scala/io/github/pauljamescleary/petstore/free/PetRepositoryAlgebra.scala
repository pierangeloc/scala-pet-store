package io.github.pauljamescleary.petstore.free

import io.github.pauljamescleary.petstore.model.Pet

object PetRepositoryAlgebra {

  sealed trait PetRepositoryAlgebraF[A]

  case class Put(pet: Pet) extends PetRepositoryAlgebraF[Pet]
  case class Get(id: Long) extends PetRepositoryAlgebraF[Option[Pet]]
  case class Delete(id: Long) extends PetRepositoryAlgebraF[Option[Pet]]
  case class FindByNameAndCategory(name: String, category: String) extends PetRepositoryAlgebraF[Set[Pet]]
  case class List(pageSize: Int, offset: Int) extends PetRepositoryAlgebraF[Seq[Pet]]

}
