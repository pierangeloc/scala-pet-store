package io.github.pauljamescleary.petstore.free

import cats.effect.IO
import io.github.pauljamescleary.petstore.free.services.IOInMemoryInterpreters
import io.github.pauljamescleary.petstore.model.Pet

object TestRunner extends App {
  import IOInMemoryInterpreters._
  import runnablePetService._

  println("inserting pets...")

  val r: IO[Unit] = for {
    _ <- run(create(Pet("Simba", "Dog", "Angel in Earth")))
    _ <- run(create(Pet("Tom", "Cat", "Not so smart")))
    _ <- run(create(Pet("Jerry", "Mouse", "Sweet and funny")))
    _ <- IO.pure{println("Listing pets...")}
    pets <- run(list(10, 0))
    _ <- IO.pure{println(pets.toList)}
  } yield ()

  r.unsafeRunSync()
}
