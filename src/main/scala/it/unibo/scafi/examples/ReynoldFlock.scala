package it.unibo.scafi.examples

import it.unibo.scafi.space.Point3D

class ReynoldFlock extends BaseMovement {
  override protected def movementLogic(): Point3D =
    rep(brownian(2))(velocity =>
      reynold(velocity, OneHopNeighbourhoodNearestN(10), OneHopNeighbourhoodWithinRange(30), 0.2, 1, 0.1)
    )
}
