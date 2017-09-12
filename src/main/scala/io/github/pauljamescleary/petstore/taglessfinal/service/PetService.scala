package io.github.pauljamescleary.petstore.taglessfinal.service

import cats.{Monad, MonadError}
import io.github.pauljamescleary.petstore.model.Pet
import io.github.pauljamescleary.petstore.taglessfinal.repository.PetRepositoryAlgebra
import io.github.pauljamescleary.petstore.taglessfinal.validation.{PetNotFoundError, PetValidationAlgebra}

import scala.language.higherKinds

/**
  *
  * @param repository where we get our data
  * @param validation something that provides validations to the service
  * @tparam F - this is the container for the things we work with, could be scala.concurrent.Future, Option, anything
  *           as long as it is a Monad
  */
class PetService[F[_]](implicit repository: PetRepositoryAlgebra[F], validation: PetValidationAlgebra[F]) {
  import cats.syntax.all._

  def create(pet: Pet)(implicit M: Monad[F]): F[Pet] = {
    for {
      _ <- validation.doesNotExist(pet)
      saved <- repository.put(pet)
    } yield saved
  }

  def get(id: Long)(implicit E: MonadError[F, Throwable]): F[Pet] = {
    repository.get(id).flatMap {
      case None => E.raiseError(PetNotFoundError(id))
      case Some(found) => E.pure(found)
    }
  }

  /* In some circumstances we may care if we actually delete the pet; here we are idempotent and do not care */
  def delete(id: Long)(implicit M: Monad[F]): F[Unit] = repository.delete(id).map(_ => ())

  def list(pageSize: Int, offset: Int): F[Seq[Pet]] = repository.list(pageSize, offset)
}