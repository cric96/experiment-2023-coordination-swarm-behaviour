package it.unibo.scafi
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.space.Point3D
import it.unibo.scafi.space.pimp._
trait PatternFormationLib extends {
  self: AggregateProgram
    with StandardSensors
    with FieldUtils
    with TimeUtils
    with BaseMovementLib
    with CustomSpawn
    with BlocksWithGC
    with ScafiAlchemistSupport
    with BlocksWithShare =>

  def line(
      leader: Boolean,
      distance: Double,
      confidence: Double,
      leaderVelocity: => Point3D = Point3D.Zero
  ): Point3D = {
    val potential = fastGradient(leader)
    val nodes = getNodeInfo(potential)
    val (left, right) = orderedNodes(nodes).splitAt(nodes.size / 2)
    val leftSuggestion = left.zipWithIndex.map { case ((id, velocity), i) =>
      id -> (Point3D(-(i + 1) * distance, 0, 0) + velocity)
    }.toMap
    val rightSuggestions = right.zipWithIndex.map { case ((id, velocity), i) =>
      id -> (Point3D((i + 1) * distance, 0, 0) + velocity)
    }.toMap
    mux(leader)(leaderVelocity) {
      val direction =
        broadcastAlongWithShare(potential, leftSuggestion ++ rightSuggestions, nbrRange).getOrElse(mid(), Point3D.Zero)
      node.put("suggestion", direction)
      mux(direction.module < confidence)(Point3D.Zero)(direction.normalize)
    }
  }

  def centeredCircle(
      leader: Boolean,
      radius: Double,
      confidence: Double,
      leaderVelocity: => Point3D = Point3D.Zero
  ): Point3D = {
    val potential = fastGradient(leader)
    val nodes = getNodeInfo(potential)
    val division = (math.Pi * 2) / nodes.size
    val suggestion = orderedNodes(nodes).zipWithIndex.map { case ((id, v), i) =>
      val angle = division * (i + 1)
      id -> (Point3D(math.sin(angle) * radius, math.cos(angle) * radius, 0) + v)
    }.toMap
    mux(leader)(leaderVelocity) {
      val direction = broadcastAlongWithShare(potential, suggestion, nbrRange).getOrElse(mid(), Point3D.Zero)
      mux(direction.module < confidence)(Point3D.Zero)(direction.normalize)
    }
  }

  def vShape(
      leader: Boolean,
      oldVelocity: Point3D,
      distance: Double,
      radius: Double,
      confidence: Double,
      leaderVelocity: Point3D = Point3D.Zero
  ): Point3D = {
    val potential = fastGradient(leader)
    val nodes = getNodeInfo(potential)
    val amount = ((Math.PI * 2) - radius) / 2 // - (Math.PI / 2)
    val leftVersor = oldVelocity.normalize.rotate(amount).rotate(-Math.PI / 2)
    val rightVersor = oldVelocity.normalize.rotate(-amount).rotate(-Math.PI / 2)
    val (left, right) = orderedNodes(nodes).splitAt(nodes.size / 2)
    val leftSuggestion = left.zipWithIndex.map { case ((id, velocity), i) =>
      id -> (leftVersor * distance * -(i + 1) + velocity)
    }.toMap
    val rightSuggestions = right.zipWithIndex.map { case ((id, velocity), i) =>
      id -> (rightVersor * distance * (i + 1) + velocity)
    }.toMap
    mux(leader)(leaderVelocity) {
      val direction =
        broadcastAlongWithShare(potential, leftSuggestion ++ rightSuggestions, nbrRange).getOrElse(mid(), Point3D.Zero)
      mux(direction.module < confidence)(Point3D.Zero)(direction.normalize)
    }

  }

  def isCircleFormed(source: Boolean, targetDistance: Double, confidence: Double): Boolean = {
    val potential = fastGradient(source, nbrRange)
    val distances = CWithShare[Double, List[Double]](potential, _ ::: _, List(potential), List.empty).filter(_ != 0.0)
    node.put("distances", distances)
    // for all distances, the distance from the leader is between the target distance and the confidence
    val isFormed = distances.forall(d => d > targetDistance - confidence && d < targetDistance + confidence)
    broadcastAlongWithShare(potential, isFormed, nbrRange)
  }

  private def getNodeInfo(potential: Double): Set[(ID, Point3D)] = {
    val distanceFromLeader = GAlongWithShare[Point3D](potential, Point3D.Zero, v => v + nbrVector(), nbrRange)
    C[Double, Set[(ID, Point3D)]](
      potential,
      (a, b) => a ++ b,
      Set((mid(), distanceFromLeader)),
      Set.empty[(ID, Point3D)]
    )
  }

  private def orderedNodes(nodes: Set[(ID, Point3D)]): List[(ID, Point3D)] = nodes
    .filter(_._1 != mid())
    .toList
    .sortBy(_._1)

}
