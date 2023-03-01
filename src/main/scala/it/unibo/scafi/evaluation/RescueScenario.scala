package it.unibo.scafi.evaluation

import it.unibo.scafi.examples.BaseMovement
import it.unibo.scafi.space.Point3D

class RescueScenario extends BaseMovement {
  def leader: Boolean = node.get("leaderId") == mid()

  def formation: Point3D = Point3D.Zero
  /*node.get("shape") match {
      case "v" => vShape(mid() == 1, )
      case "line" => line(mid() == 1, 1, 0.1)
    }*/

  def isFormed: Boolean = false

  def firstTarget: Point3D = Point3D.Zero

  def secondTarget: Point3D = Point3D.Zero

  override protected def movementLogic(): Point3D =
    /*execute.once {
      plan()
    }*/
    Point3D.Zero
}
