package it.unibo.scafi

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.space.Point3D
import it.unibo.scafi.space.pimp.PimpPoint3D
trait LeaderBasedLib {
  self: AggregateProgram
    with StandardSensors
    with FieldUtils
    with TimeUtils
    with BaseMovementLib
    with CustomSpawn
    with BlocksWithGC
    with BlocksWithShare
    with ScafiAlchemistSupport =>

  def alignWithLeader(source: Boolean, point: Point3D): Point3D =
    GWithShare(source, point, identity[Point3D], nbrRange)

  def sinkAt(source: Boolean): Point3D =
    GWithShare[Point3D](source, Point3D.Zero, vector => vector + nbrVector(), nbrRange).normalize

  def spinAround(center: Boolean): Point3D = {
    val toCenter = sinkAt(center)
    toCenter.crossProduct(Point3D(0, 0, 1))
  }
}
