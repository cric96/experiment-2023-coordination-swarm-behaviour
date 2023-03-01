package it.unibo.scafi

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.space.Point3D
import it.unibo.scafi.space.pimp._

trait FlockLib extends ScafiAlchemistSupport {
  self: AggregateProgram
    with StandardSensors
    with FieldUtils
    with TimeUtils
    with BaseMovementLib
    with CustomSpawn
    with BlocksWithGC
    with BlocksWithShare =>

  def vicsek(
      velocity: Point3D,
      neighbouringQuery: NeighbouringQuery,
      alpha: Double = 0.1,
      epsilon: Double = 0.1
  ): Point3D = {
    val neighbours = neighbouringQuery.queryLocal(velocity)
    val averageVelocity = neighbours.values.reduce(_ + _) / neighbours.size.toDouble
    val randomPerturbation = brownian(epsilon)
    velocity * (1 - alpha) + averageVelocity * alpha + randomPerturbation
  }

  def cuckerSmale(
      velocity: Point3D,
      neighbouringQuery: NeighbouringQuery,
      strength: Double,
      epsilon: Double = 0.1
  ): Point3D = {
    val neighbours = neighbouringQuery.query(velocity, 0.0, (d: Double) => nbrRange() + d).filter(_._1 != mid())
    val perturbation = brownian(epsilon)
    if (neighbours.isEmpty) {
      velocity + perturbation
    } else {
      neighbours
        .map { case (_, (velocityNeighbour, distance)) => (velocityNeighbour - velocity) * (strength / distance) }
        .foldLeft(velocity + perturbation)(_ + _)
    }
  }

  def reynold(
      velocity: Point3D,
      visionRange: NeighbouringQuery,
      separationRange: NeighbouringQuery,
      separationFactor: Double,
      alignFactor: Double,
      cohesionFactor: Double
  ): Point3D = {
    val separationForce = separation(velocity, separationRange)
    val alignForce = align(velocity, visionRange)
    val cohesionForce = cohesion(velocity, visionRange)
    (separationForce.normalize * separationFactor + alignForce.normalize * alignFactor + cohesionForce.normalize * cohesionFactor) + velocity
  }

  def separation(velocity: Point3D, neighborhood: NeighbouringQuery): Point3D = {
    val distances = neighborhood.queryAccum(Point3D.Zero, distance => nbrVector() + distance).withoutMe()
    separationFromDistancesVector(velocity, distances.values.map(Point3D.Zero - _))
  }

  def align(velocity: Point3D, neighborhood: NeighbouringQuery): Point3D = {
    val neighbourhood = neighborhood.queryLocal(velocity).withoutMe()
    if (neighbourhood.isEmpty) {
      Point3D.Zero
    } else {
      (neighbourhood.values.reduce(_ + _) / neighbourhood.size.toDouble).normalize - velocity
    }
  }

  def cohesion(velocity: Point3D, neighborhood: NeighbouringQuery): Point3D = {
    val distances = neighborhood.queryAccum(Point3D.Zero, distance => nbrVector() + distance).withoutMe()
    if (distances.isEmpty) {
      Point3D.Zero
    } else {
      (distances.values.reduce(_ + _) / distances.size.toDouble).normalize - velocity
    }
  }
  /*
  def separationWithGPS(velocity: Point3D, neighborhood: NeighbouringQuery): Point3D = {
    val distances = neighborhood.queryLocal(currentPosition()).filter(_._1 != mid()).map(position => currentPosition() - position._2)
    separationFromDistancesVector(velocity, distances)
  }*/

  private def separationFromDistancesVector(velocity: Point3D, distances: Iterable[Point3D]): Point3D = {
    if (distances.isEmpty) {
      Point3D.Zero
    } else {
      val separationForce = distances.map(point => point).reduce(_ + _) / distances.size
      node.put("separation", separationForce)
      separationForce.normalize - velocity
    }
  }
  trait NeighbouringQuery {
    def queryLocal[L](local: L): Map[Int, L] = query(local, (), (_: Unit) => ()).view.mapValues(_._1).toMap
    def queryAccum[A](center: A, accum: A => A): Map[Int, A] = query((), center, accum).view.mapValues(_._2).toMap
    def query[L, A](local: L, valueAtCenter: A, accum: A => A): Map[Int, (L, A)]
  }

  case object OneHopNeighbourhood extends NeighbouringQuery {
    override def query[L, A](local: L, valueAtCenter: A, accum: A => A): Map[Int, (L, A)] =
      includingSelf.reifyField((nbr(local), accum(valueAtCenter)))
  }

  case class OneHopNeighbourhoodWithinRange(range: Double) extends NeighbouringQuery {
    override def query[L, A](local: L, valueAtCenter: A, accum: A => A): Map[Int, (L, A)] =
      includingSelf
        .reifyField((nbr(local), accum(valueAtCenter), nbrRange()))
        .filter(_._2._3 < range)
        .view
        .mapValues { case (local, acc, _) => (local, acc) }
        .toMap
  }

  case class OneHopNeighbourhoodNearestN(k: Int) extends NeighbouringQuery {
    override def query[L, A](local: L, valueAtCenter: A, accum: A => A): Map[Int, (L, A)] =
      includingSelf
        .reifyField((nbr(local), accum(valueAtCenter), nbrRange()))
        .toList
        .sortBy(_._2._3)
        .map { case (id, (local, acc, _)) => id -> (local, acc) }
        .take(k)
        .toMap
  }
  case class LongRangeNeighborhood(radius: Double) extends NeighbouringQuery {
    override def query[L, A](local: L, valueAtCenter: A, accum: A => A): Map[Int, (L, A)] =
      sspawn2[Int, Unit, Map[Int, (L, A)]](
        id =>
          (_: Unit) => {
            val source = mid() == id
            val potential = fastGradient(source, nbrRange)
            val accumValue: A = GAlongWithShare[A](potential, valueAtCenter, accum, nbrRange)
            mux(potential < radius) {
              val result = CWithShare[Double, Map[Int, (L, A)]](
                potential,
                (acc, local) => acc ++ local,
                Map((mid(), (local, accumValue))),
                Map.empty
              )
              POut(result, SpawnInterface.OutputStatus)
            } {
              POut(Map.empty, SpawnInterface.ExternalStatus)
            }
          },
        Set(mid),
        ()
      ).getOrElse(mid(), Map.empty)
  }

  implicit class MapExcludingMe[V](map: Map[Int, V]) {
    def withoutMe(): Map[Int, V] = map.filter(_._1 != mid())
  }
}
