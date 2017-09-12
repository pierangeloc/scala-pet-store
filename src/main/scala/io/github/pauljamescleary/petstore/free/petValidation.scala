package io.github.pauljamescleary.petstore.free

import cats.effect.IO
import cats.free.Free
import cats.~>
import io.github.pauljamescleary.petstore.free.PetValidationAlgebra.PetValidationAlgebraF
import io.github.pauljamescleary.petstore.model.Pet

object PetValidationAlgebra {

  sealed trait PetValidationAlgebraF[A]
  case class DoesNotExist(pet: Pet) extends PetValidationAlgebraF[Unit]

}

trait PetValid {
  import PetValidationAlgebra._
  type PetValidation[A] = Free[PetValidationAlgebraF, A]

  def doesNotExist(pet: Pet): PetValidation[Unit] = Free.liftF(DoesNotExist(pet))
}

trait PetValidIOInterpreter extends PetValid {

  def natTrans: PetValidationAlgebraF ~> IO

  def apply[A](program: PetValidation[A]): IO[A] = program.foldMap(natTrans)
}