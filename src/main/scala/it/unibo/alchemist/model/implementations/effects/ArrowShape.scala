package it.unibo.alchemist.model.implementations.effects

import it.unibo.alchemist.boundary.swingui.effect.api.Effect
import it.unibo.alchemist.boundary.ui.api.Wormhole2D
import it.unibo.alchemist.boundary.wormhole.impl.WormholeSwing
import it.unibo.alchemist.model.implementations.effects.ArrowShape._
import it.unibo.alchemist.model.implementations.molecules.SimpleMolecule
import it.unibo.alchemist.model.implementations.nodes._
import it.unibo.alchemist.model.interfaces.{Environment, Node, Position, Position2D}
import it.unibo.scafi.space.Point3D
import it.unibo.scafi.space.pimp.PimpPoint3D

import java.awt._
import java.awt.geom._
import scala.collection.mutable

/** ad-hoc effect to draw drones, animal and station. */
class ArrowShape extends Effect {

  override def apply[T, P <: Position2D[P]](
      g: Graphics2D,
      node: Node[T],
      env: Environment[T, P],
      wormhole: Wormhole2D[P]
  ): Unit = {
    val nodePosition: P = env.getPosition(node)
    val viewPoint: Point = wormhole.getViewPoint(nodePosition)
    val (x, y) = (viewPoint.x, viewPoint.y)
    drawArrow(g, node, x, y, wormhole.getZoom)
  }

  def getColorSummary: Color = Color.GREEN

  def drawArrow[T, P <: Position[P]](
      g: Graphics2D,
      droneNode: Node[T],
      x: Int,
      y: Int,
      zoom: Double
  ): Unit = {
    val transform = getTransform(x, y, DRONE_SIZE, rotation(droneNode))
    val shape = DRONE_SHAPE
    val transformedShape = transform.createTransformedShape(shape)
    g.setColor(DRONE_COLOR)
    g.fill(transformedShape)
  }

  private def rotation[T](node: Node[T]): Double = {
    val nodeManager = new SimpleNodeManager[T](node)
    val velocity = nodeManager.getOption("velocity").getOrElse(Point3D(1, 0, 0))
    val smoothed = nodeManager
      .getOption[Point3D]("old")
      .map(_ + (velocity / SMOOTHING_FACTOR))
      .map(_.normalize)
      .getOrElse(velocity)
    nodeManager.put("old", smoothed)
    math.atan2(smoothed.x, smoothed.y)
  }

  private def getTransform(x: Int, y: Int, zoom: Double, rotation: Double): AffineTransform = {
    val transform = new AffineTransform()
    transform.translate(x, y)
    transform.scale(zoom, zoom)
    transform.rotate(rotation)
    transform
  }
}

object ArrowShape {

  val MAX_COLOR: Int = 100

  val TRANSPARENT = new Color(255, 255, 255, 0)

  val ANIMAL_COLOR_CACHE: mutable.Map[String, Color] = new mutable.HashMap()

  val DRONE_SHAPE: Polygon = new Polygon(Array(-1, 0, 1), Array(2, -2, 2), 3)

  val DRONE_SIZE = 4.0

  val DRONE_COLOR: Color = Color.BLACK

  val SMOOTHING_FACTOR = 10
}
