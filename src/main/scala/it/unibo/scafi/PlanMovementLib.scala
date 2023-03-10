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

  class PlanExecutor(plans: Seq[Plan], repeated: Boolean = false) {
    def run(): Point3D = {
      rep((0, Point3D.Zero)) { case (planId, _) =>
        // val velocities = plans.map(_.computation())
        // val conditions = plans.map(_.condition())
        /*val (newId, v) = align(planId) { planId =>
          val condition = plans(planId).condition()
          val velocity = plans(planId).computation()
          node.put("plan", planId)
          val (_, stable) = rep((-1, false)) { case (oldNeigh, _) =>
            val current = foldhood(0)(_ + _)(1)
            (current, current == oldNeigh)
          }
          (mux(condition && stable)(planId + 1)(planId), velocity)
        }

        branch(plans.length > newId) {
          (newId, v)
        }(
          (
            if (repeated) { 0 }
            else { planId },
            Point3D.Zero
          )
        )*/
        // val conditions = plans.map(_.condition())
        // val velocities = plans.map(_.computation())
        node.put("planId", planId)
        branch(plans.length > planId) {
          val conditions = plans.map(_.condition())
          val velocities = plans.map(_.computation())
          val condition = conditions(planId)
          val velocity = velocities(planId)
          val (_, stable) = rep((-1, false)) { case (oldNeigh, _) =>
            val current = foldhood(0)(_ + _)(1)
            (current, current == oldNeigh)
          }
          (mux(condition && stable)(planId + 1)(planId), velocity)
        }(
          (
            if (repeated) { 0 }
            else { planId },
            Point3D.Zero
          )
        )
      }
    }._2
  }

  def plan(velocity: => Point3D): PlanBuilder = new PlanBuilder(() => velocity)

  class PlanBuilder(velocity: () => Point3D) {
    def endWhen(condition: => Boolean): Plan = Plan(velocity, () => condition)
  }

  object execute {
    def once(plans: Plan*): PlanExecutor = new PlanExecutor(plans)
    def repeat(plans: Plan*): PlanExecutor = new PlanExecutor(plans, true)
  }
}
