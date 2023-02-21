package it.unibo.scafi
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.space.Point3D
import it.unibo.scafi.space.optimization.RichPoint3D
//import it.unibo.scafi.space.pimp._
trait GPSMovement {
  self: StandardSensors with AggregateProgram with TimeUtils =>

  def goto(destination: P, maxVelocity: Double = 1): P = {
    val distance = currentPosition().distance(destination)
    val direction = (destination: Point3D) - (currentPosition: Point3D)
    val velocity = direction * (1 / distance)
    velocity * maxVelocity
  }

  def explore(minBound: P, maxBound: P, maxVelocity: Double): P = {
    def randomInBound(): P = {
      val x = randomGenerator().nextDouble() * (maxBound.x - minBound.x) + minBound.x
      val y = randomGenerator().nextDouble() * (maxBound.y - minBound.y) + minBound.y
      val z = randomGenerator().nextDouble() * (maxBound.z - minBound.z) + minBound.z
      Point3D(x, y, z)
    }
    val goal = rep(randomInBound())(goal => mux(isClose(goal))(randomInBound())(goal))
    goto(goal, maxVelocity)
  }

  def isClose(goal: P, dist: Double = 10): Boolean =
    currentPosition().distance(goal) <= dist
}
