package it.unibo.alchemist.loader.`export`.extractors

import it.unibo.alchemist.loader.`export`.Extractor
import it.unibo.alchemist.model.implementations.molecules.SimpleMolecule
import it.unibo.alchemist.model.interfaces
import it.unibo.alchemist.model.interfaces.{Actionable, Environment}

import java.util
import scala.jdk.CollectionConverters.ListHasAsScala

class InDangerExtractor extends Extractor[Double] {
  override def getColumnNames: util.List[String] = util.List.of("inDanger", "dangerTriggered")

  override def extractData[T](
      environment: Environment[T, _],
      actionable: Actionable[T],
      time: interfaces.Time,
      l: Long
  ): util.Map[String, Double] = {
    val dangerTriggered = environment.getNodes.asScala
      .filter(node => node.contains(new SimpleMolecule("danger")))
    val inDanger = dangerTriggered.filter(n => n.getConcentration(new SimpleMolecule("danger")).asInstanceOf[Boolean])
    util.Map.of("inDanger", inDanger.size, "dangerTriggered", dangerTriggered.size)
  }

}
