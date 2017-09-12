package io.github.pauljamescleary.petstore.endpoint

import cats.effect._
import io.circe._
import io.circe.generic.auto._
import io.circe.generic.extras.semiauto._
import io.circe.syntax._
import io.github.pauljamescleary.petstore.model.{Pet, Status}
import io.github.pauljamescleary.petstore.taglessfinal.service.PetService
import io.github.pauljamescleary.petstore.taglessfinal.validation.{PetAlreadyExistsError, PetNotFoundError}
import org.http4s.circe._
import org.http4s.dsl._
import org.http4s.{HttpService, QueryParamDecoder}

import scala.language.higherKinds

object PetEndpoints {

  /* Necessary for decoding query parameters */
  import QueryParamDecoder._

  /* Needed for service composition via |+| */
  import cats.implicits._

  /* Parses out the id query param */
  object IdMatcher extends QueryParamDecoderMatcher[Long]("id")

  /* Parses out the offset and page size params */
  object PageSizeMatcher extends QueryParamDecoderMatcher[Int]("pageSize")
  object OffsetMatcher extends QueryParamDecoderMatcher[Int]("offset")

  /* We need to define an enum encoder and decoder since these do not come out of the box with generic derivation */
  implicit val statusDecoder = deriveEnumerationDecoder[Status]
  implicit val statusEncoder = deriveEnumerationEncoder[Status]

  private def createPetEndpoint(petService: PetService[IO]): HttpService[IO] = HttpService[IO] {
    case req@POST -> Root / "pets" => {
      for {
        pet <- req.as(implicitly, jsonOf[IO, Pet]) // <-- TODO: Make this cleaner in HTTP4S
        saved <- petService.create(pet)
        resp <- Ok(saved.asJson)
      } yield resp
    }.handleErrorWith {
      case PetAlreadyExistsError(pet) => Conflict(s"The pet ${pet.name} of category ${pet.category} already exists")
    }
  }

  private def getPetEndpoint(petService: PetService[IO]): HttpService[IO] = HttpService[IO] {
    case GET -> Root / "pets" :? IdMatcher(id) => {
      for {
        retrieved <- petService.get(id)
        resp <- Ok(retrieved.asJson)
      } yield resp
    }.handleErrorWith {
      case PetNotFoundError(notFound) => NotFound(s"The pet with id $notFound was not found")
    }
  }

  private def deletePetEndpoint(petService: PetService[IO]): HttpService[IO] = HttpService[IO] {
    case DELETE -> Root / "pets" :? IdMatcher(id) =>
      for {
        _ <- petService.delete(id)
        resp <- Ok()
      } yield resp
  }

  private def listPetsEndpoint(petService: PetService[IO]): HttpService[IO] = HttpService[IO] {
    case GET -> Root / "pets" :? PageSizeMatcher(pageSize) :? OffsetMatcher(offset) =>
      for {
        retrieved <- petService.list(pageSize, offset)
        resp <- Ok(retrieved.asJson)
      } yield resp
  }

  def endpoints(petService: PetService[IO]): HttpService[IO] =
    createPetEndpoint(petService) |+| getPetEndpoint(petService) |+| deletePetEndpoint(petService) |+| listPetsEndpoint(petService)
}
