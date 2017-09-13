package io.github.pauljamescleary.petstore.free

import io.github.pauljamescleary.petstore.model.Pet

object PetValidationAlgebra {

  sealed trait PetValidationAlgebraF[A]
  case class DoesNotExist(pet: Pet) extends PetValidationAlgebraF[Unit]

}