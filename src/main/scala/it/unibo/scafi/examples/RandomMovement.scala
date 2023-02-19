package it.unibo.scafi.examples
import it.unibo.scafi.space.Point3D
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

class RandomMovement extends BaseMovement {
  override protected def movementLogic(): Point3D = maintainTrajectory(remember(brownian(2)))(100 seconds)
}
