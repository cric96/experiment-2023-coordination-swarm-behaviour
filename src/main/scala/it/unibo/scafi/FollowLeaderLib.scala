package it.unibo.scafi

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.space.Point3D
import it.unibo.scafi.space.pimp.PimpPoint3D
trait FollowLeaderLib {
  self: AggregateProgram
    with StandardSensors
    with FieldUtils
    with TimeUtils
    with BaseMovementLib
    with CustomSpawn
    with BlocksWithGC
    with BlocksWithShare
    with FlockLib
    with ScafiAlchemistSupport =>

  def alignWithLeader(source: Boolean, point: Point3D): Point3D =
    GWithShare(source, point, identity[Point3D], nbrRange)

  def sinkAt(source: Boolean): Point3D =
    GWithShare[Point3D](source, Point3D.Zero, vector => vector + nbrVector(), nbrRange).normalize

  def spinAround(center: Boolean): Point3D = {
    val toCenter = sinkAt(center)
    crossProduct(toCenter, Point3D(0, 0, 1))
  }

  private def crossProduct(p: Point3D, other: Point3D): Point3D =
    Point3D(p.y * other.z - p.z * other.y, p.z * other.x - p.x * other.z, p.x * other.y - p.y * other.x)

  def teamFormed(source: Boolean, targetDistance: Double, necessary: Int = 1): Boolean = {
    val potential = fastGradient(source, nbrRange)
    val totalDistance = excludingSelf.reifyField(nbrRange())
    val averageDistance = totalDistance.values.toList.sorted
      .take(necessary)
      .reduceOption(_ + _)
      .map(_ / necessary)
      .getOrElse(Double.PositiveInfinity)
    val isFormed = CWithShare[Double, Boolean](potential, _ && _, averageDistance <= targetDistance, true)
    broadcastAlongWithShare(potential, isFormed, nbrRange)
  }

  def countIn(source: Boolean): Int = {
    val potential = fastGradient(source, nbrRange)
    val count = CWithShare[Double, Int](potential, _ + _, 1, 0)
    broadcastAlongWithShare(potential, count, nbrRange)
  }

  case class Team(leader: ID, isFormed: Boolean, velocity: Point3D) {
    def insideTeam(velocityGenerator: ID => Point3D): Point3D = rep((isFormed, velocity)) { case (formed, _) =>
      branch(!formed)((isFormed, velocity))(align(leader)(k => (true, velocityGenerator(k))))
    }._2
  }
  /*
  def teamFormation(
      targetIntraDistance: Double,
      targetExtraDistance: Double,
      confidence: Double,
      separationWeight: Double,
      necessary: Int = 1
  ): Team = {
    val leader = SWithShare(targetExtraDistance, nbrRange)
    val localLeader = broadcastAlongWithShare(fastGradient(leader, nbrRange), mid(), nbrRange)
    val isFormed = teamFormed(leader, targetIntraDistance + confidence, necessary)
    val velocity = rep(Point3D.Zero)(velocity =>
      branch(!isFormed) {
        (sinkAt(leader) + separation(
          velocity,
          OneHopNeighbourhoodWithinRange(targetIntraDistance)
        ) * separationWeight).normalize
      }(Point3D.Zero)
    )
    Team(localLeader, isFormed, velocity)
  }*/

  def teamFormation(
      targetIntraDistance: Double,
      targetExtraDistance: Double,
      separationWeight: Double,
      condition: Boolean => Boolean
  ): Team = {

    val (leaderId, formed, velocity) = rep((mid(), false, Point3D.Zero)) { case (leaderId, formed, velocity) =>
      branch(!formed) {
        val leader = SWithShare(targetExtraDistance, nbrRange)
        val localLeader = broadcastAlongWithShare(fastGradient(leader, nbrRange), mid(), nbrRange)
        val isFormed = condition(leader)
        val updateVelocity = (sinkAt(leader) + separation(
          velocity,
          OneHopNeighbourhoodWithinRange(targetIntraDistance)
        ) * separationWeight).normalize
        (localLeader, isFormed, updateVelocity)
      } {
        (leaderId, true, Point3D.Zero)
      }
    }
    Team(leaderId, formed, velocity)
  }
}
