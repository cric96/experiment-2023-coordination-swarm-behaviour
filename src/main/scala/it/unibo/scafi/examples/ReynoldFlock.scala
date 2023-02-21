package it.unibo.scafi.examples

import it.unibo.scafi.space.Point3D

class ReynoldFlock extends BaseMovement {
  override protected def movementLogic(): Point3D =
    rep(brownian(2))(velocity =>
      reynold(
        velocity,
        visionRange = OneHopNeighbourhoodNearestN(10),
        separationRange = OneHopNeighbourhoodWithinRange(30),
        separationFactor = 0.2,
        alignFactor = 1,
        cohesionFactor = 0.1
      )
    )
}
