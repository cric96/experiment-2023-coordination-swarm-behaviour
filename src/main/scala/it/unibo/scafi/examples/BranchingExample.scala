package it.unibo.scafi.examples

import it.unibo.scafi.space.Point3D

import scala.language.postfixOps

class BranchingExample extends BaseMovement {
  override protected def movementLogic(): Point3D = {
    def logic = alignWithLeader(mid() == 1 || mid() == 40, remember(brownian(2)))
    branch(mid() < 25)(logic)(logic)
  }
}
