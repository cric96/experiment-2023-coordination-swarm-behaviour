package it.unibo.scafi

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.space.Point3D
import it.unibo.scafi.space.pimp._

import scala.concurrent.duration.FiniteDuration
trait BaseMovementLib {
  self: StandardSensors with AggregateProgram with TimeUtils with BlocksWithShare =>

  def standStill(): P = Point3D.Zero

  def brownian(scale: Double = 1.0): P =
    randomInNegativeUnitSphere(scale)

  def maintainTrajectory(velocityGenerator: => P)(time: FiniteDuration): P =
    rep(velocityGenerator)(previousVelocity => mux(impulsesEvery(time))(velocityGenerator)(previousVelocity))

  def maintainUntil(velocity: P)(condition: Boolean): P =
    mux(condition)(Point3D.Zero)(velocity)

  def obstacleAvoidance(
      obstacles: Seq[Point3D],
      minDistance: Double,
      distanceWeightNormalization: Double = 100
  ): Point3D = {
    -obstacles
      .map(obstacleDirection => (minDistance - obstacleDirection.module, obstacleDirection.normalize))
      .filter(_._1 > 0)
      .map { case (magnitude, direction) =>
        direction * (magnitude / distanceWeightNormalization)
      }
      .foldLeft(Point3D.Zero)(_ + _)
  }
  /*-(obstacles
      .minByOption(_.module)
      .map(distance => distance.normalize * (weight / distance.module))
      .getOrElse(Point3D.Zero): Point3D)*/

  private def randomInNegativeUnitSphere(scale: Double): P = {
    val x = randomGenerator().nextDouble() * 2 - 1
    val y = randomGenerator().nextDouble() * 2 - 1
    val z = randomGenerator().nextDouble() * 2 - 1
    Point3D(x * scale, y * scale, z * scale)
  }
}
