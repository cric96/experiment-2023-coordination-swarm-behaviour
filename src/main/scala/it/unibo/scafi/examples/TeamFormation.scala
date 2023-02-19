package it.unibo.scafi.examples

import it.unibo.scafi.space.Point3D
import it.unibo.scafi.space.pimp.PimpPoint3D

import scala.language.postfixOps

class TeamFormation extends BaseMovement {
  override protected def movementLogic(): Point3D = {
    val team = teamFormation(
      30,
      300,
      2,
      leader => teamFormed(leader, 40) & countIn(leader) > 3
    )
    node.put("leader", mid() == team.leader)
    team.insideTeam(k => alignWithLeader(mid() == k, explore(Point3D(0, 0, 0), Point3D(1000, 1000, 0), 1))).normalize
  }
}
