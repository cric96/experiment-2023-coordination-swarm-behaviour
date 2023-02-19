package it.unibo.scafi.examples

import it.unibo.alchemist.model.interfaces.Position
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.ScafiAlchemistSupport
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.space.Point3D
import it.unibo.scafi.{
  BaseMovementLib,
  BlocksWithShare,
  FlockLib,
  FollowLeaderLib,
  GPSMovement,
  PlanMovementLib,
  ProcessFix
}

trait BaseMovement
    extends AggregateProgram
    with BaseMovementLib
    with StandardSensors
    with ScafiAlchemistSupport
    with FieldUtils
    with TimeUtils
    with GPSMovement
    with FlockLib
    with CustomSpawn
    with BlocksWithGC
    with BlocksWithShare
    with ProcessFix
    with FollowLeaderLib
    with PlanMovementLib
    with BlockS {

  override def main(): Any = actuate(movementLogic())

  protected def movementLogic(): Point3D

  protected def actuate(velocity: Point3D): Unit = {
    val previousPosition = alchemistEnvironment.getPosition(alchemistEnvironment.getNodeByID(mid()))
    val target = previousPosition.plus(Array(velocity.x, velocity.y))
    target.asInstanceOf[Position[_]]
    node.put("velocity", velocity)
    node.put("destination", target)
  }
}
