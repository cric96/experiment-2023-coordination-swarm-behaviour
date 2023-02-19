package it.unibo.scafi

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import Builtins._
trait BlocksWithShare {
  self: AggregateProgram with ProcessFix with StandardSensors with BlocksWithGC with ScafiAlchemistSupport =>

  def fastGradient(source: Boolean, metric: Metric = nbrRange): Double = {
    share(Double.PositiveInfinity) { case (l, nbrg) =>
      mux(source)(0.0)(minHoodPlus(nbrg() + metric()))
    }
  }

  def GAlongWithShare[V](g: Double, field: V, acc: V => V, metric: Metric = nbrRange): V = {
    share(field) { case (_, nbrField) =>
      mux(g == 0.0)(field) {
        excludingSelf.minHoodSelector[Double, V](nbr(g) + metric())(acc(nbrField())).getOrElse(field)
      }
    }
  }

  def GInverseAlongWithShare[V](g: Double, field: V, acc: V => V, metric: Metric = nbrRange): V = {
    share(field) { case (_, nbrField) =>
      node.put("field", field)
      excludingSelf.maxHoodSelector[Double, V](nbr(g) + metric())(acc(nbrField())).getOrElse(field)
    }
  }

  def CWithShare[P: Bounded, V](potential: P, acc: (V, V) => V, local: V, Null: V): V =
    share(local) { (_, nbrv) =>
      acc(
        local,
        foldhood(Null)(acc) {
          mux(nbr(findParent(potential)) == mid())(nbrv())(nbr(Null))
        }
      )
    }

  def GWithShare[V](source: Boolean, field: V, acc: V => V, metric: Metric = nbrRange): V =
    share((Double.MaxValue, field)) { case ((dist, value), nbrvalues) =>
      mux(source) {
        (0.0, field)
      } {
        excludingSelf
          .minHoodSelector(nbrvalues()._1 + metric())((nbrvalues()._1 + metric() + metric(), acc(nbrvalues()._2)))
          .getOrElse((Double.PositiveInfinity, field))
      }
    }._2

  def broadcastAlongWithShare[V](g: Double, value: V, metric: Metric = nbrRange) =
    GAlongWithShare(g, value, identity[V], metric)

  def SWithShare(grain: Double, metric: Metric): Boolean =
    breakUsingUidsWithShare(randomUid, grain, metric)

  /** Generates a field of random unique identifiers.
    *
    * @return
    *   a tuple where the first element is a random number, end the second element is the device identifier to ensure
    *   uniqueness of the field elements.
    */
  private def randomUid: (Double, ID) = rep((nextRandom(), mid())) { v =>
    (v._1, mid())
  }

  /** Breaks simmetry using UIDs. UIDs are used to break symmetry by a competition between devices for leadership. */
  def breakUsingUidsWithShare(uid: (Double, ID), grain: Double, metric: Metric): Boolean =
    // Initially, each device is a candidate leader, competing for leadership.
    uid == (share((uid, 0.0)) { (lead, nbrInfo) =>
      // Distance from current device (uid) to the current leader (lead).
      val dist = GWithShare[Double](uid == lead._1, 0, (_: Double) + metric(), metric)
      // Initially, current device is candidate, so the distance ('dist')
      // will be 0; the same will be for other devices.
      // To solve the conflict, devices abdicate in favor of devices with
      // lowest UID, according to 'distanceCompetition'.
      (distanceCompetitionWithShare(nbrInfo, lead, grain, metric), dist)
    }._1)

  /** Candidate leader devices surrender leadership to the lowest nearby UID.
    *
    * @return
    */
  def distanceCompetitionWithShare(
      nbrInfo: () => ((Double, ID), Double),
      localInfo: ((Double, ID), Double),
      grain: Double,
      metric: Metric
  ): (Double, ID) = {
    val inf: (Double, ID) = (Double.PositiveInfinity, localInfo._1._2)
    mux(localInfo._2 > grain) {
      // If the current device has a distance to the current candidate leader
      //   which is > grain, then the device candidate itself for another region.
      // Remember: 'grain' represents, in the algorithm,
      //   the mean distance between two leaders.
      localInfo._1
    } {
      mux(localInfo._2 >= (0.5 * grain)) {
        // If the current device is at an intermediate distance to the
        //   candidate leader, then it abdicates (by returning 'inf').
        inf
      } {
        // Otherwise, elect the leader with lowest UID.
        // Note: it works because Tuple2 has an OrderingFoldable where
        //   the min(t1,t2) is defined according the 1st element, or
        //   according to the 2nd elem in case of breakeven on the first one.
        //   (minHood uses min to select the candidate leader tuple)
        minHood {
          mux(nbrInfo()._2 + metric() >= 0.5 * grain) {
            nbr(inf)
          } {
            nbrInfo()._1
          }
        }
      }
    }
  }

}
