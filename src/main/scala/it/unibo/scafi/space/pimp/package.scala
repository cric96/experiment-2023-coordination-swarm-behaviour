package it.unibo.scafi.space

package object pimp {
  implicit class PimpPoint3D(p: Point3D) {
    def *(alpha: Double): Point3D = Point3D(p.x * alpha, p.y * alpha, p.z * alpha)
    def /(alpha: Double): Point3D = Point3D(p.x / alpha, p.y / alpha, p.z / alpha)
    def -(p2: Point3D): Point3D = Point3D(p.x - p2.x, p.y - p2.y, p.z - p2.z)
    def module: Double = Math.sqrt(p.x * p.x + p.y * p.y + p.z * p.z)
    def normalize: Point3D = if (p == Point3D.Zero) { p }
    else { p / p.module }
    def unary_- : Point3D = Point3D(-p.x, -p.y, -p.z)
  }

  implicit class RichDoublePointContext(p: Double) {
    def /(p2: Point3D): Point3D = Point3D(p / p2.x, p / p2.y, p / p2.z)
  }
}
