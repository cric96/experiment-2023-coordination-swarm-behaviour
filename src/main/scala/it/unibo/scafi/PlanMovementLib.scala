package it.unibo.scafi

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.ScafiAlchemistSupport
import it.unibo.scafi.space.Point3D

trait PlanMovementLib {
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

  case class Plan(computation: () => Point3D, condition: () => Boolean)

  class PlanExecutor(plans: Seq[Plan]) {
    def execute(): Point3D = {
      rep((0, Point3D.Zero)) { case (planId, _) =>
        val velocities = plans.map(_.computation())
        val conditions = plans.map(_.condition())
        branch(conditions.length > planId) {
          val condition = conditions(planId)
          val velocity = velocities(planId)
          (mux(condition)(planId + 1)(planId), velocity)
        }((planId, Point3D.Zero))
      }
    }._2
  }

  def plan(velocity: => Point3D): PlanBuilder = new PlanBuilder(() => velocity)

  class PlanBuilder(velocity: () => Point3D) {
    def endWhen(condition: => Boolean): Plan = Plan(velocity, () => condition)
  }

  object plans {
    def once(plans: Plan*): PlanExecutor = new PlanExecutor(plans)
  }
}
