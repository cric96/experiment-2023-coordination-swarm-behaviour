package it.unibo.alchemist.loader.`export`.exporters

import it.unibo.alchemist.model.interfaces.{Actionable, Environment, Position, Time}

import java.awt.Color
import java.awt.image.BufferedImage

class SnapshotExporter[T, P <: Position[P]](
    val path: String,
    val name: String,
    val samplingInterval: Double,
    val maxSize: Int
) extends AbstractExporter[T, P](samplingInterval: Double) {
  var snapshots: List[BufferedImage] = List.empty
  override def exportData(environment: Environment[T, P], actionable: Actionable[T], time: Time, l: Long): Unit = {
    // get the current active windows of a swing application
    val currentWindow = java.awt.Window.getWindows.headOption
    currentWindow.foreach { currentWindow =>
      val rootPane = currentWindow.getComponents.head.asInstanceOf[javax.swing.JRootPane]
      val contentPane = rootPane.getContentPane.asInstanceOf[javax.swing.JPanel]
      val simulationPane = contentPane.getComponents.head.asInstanceOf[javax.swing.JPanel]
      val simulationDisplay = simulationPane.getComponents.head
      // create an image with the size of the current container
      val image = new java.awt.image.BufferedImage(
        simulationDisplay.getWidth,
        simulationDisplay.getHeight,
        java.awt.image.BufferedImage.TYPE_INT_ARGB
      )
      simulationDisplay.paint(image.getGraphics)
      // snapshots = (image :: snapshots).take(maxSize)
      // save the image to a file
      javax.imageio.ImageIO.write(image, "png", new java.io.File(path + "/" + name + time.toDouble + ".png"))
    // processSnapshotIntoOneImage()
    }
  }

  def processSnapshotIntoOneImage(): Unit = {
    // for each image, convert the white pixel to transparent
    val transparentSnapshots = snapshots.reverse.map { snapshot =>
      for {
        x <- 0 until snapshot.getWidth
        y <- 0 until snapshot.getHeight
      } {
        val color = new java.awt.Color(snapshot.getRGB(x, y), true)
        if (color.getRed == 255 && color.getGreen == 255 && color.getBlue == 255) {
          snapshot.setRGB(x, y, new Color(0, 0, 0, 0).getRGB)
        }
      }
      snapshot
    }
    // reduce the alpha value of each pixel with respect to the number of images
    val alpha = 255 / transparentSnapshots.size
    transparentSnapshots.zipWithIndex.foreach { case (snapshot, i) =>
      for {
        x <- 0 until snapshot.getWidth
        y <- 0 until snapshot.getHeight
      } {
        val color = new java.awt.Color(snapshot.getRGB(x, y), true)
        if (color.getAlpha != 0) {
          val newColor = new java.awt.Color(color.getRed, color.getGreen, color.getBlue, alpha * (i + 1))
          snapshot.setRGB(x, y, newColor.getRGB)
        }
      }
    }
    // create a new image with the size of the first image
    val firstImage = transparentSnapshots.head
    val finalImage = new BufferedImage(firstImage.getWidth, firstImage.getHeight, BufferedImage.TYPE_INT_ARGB)
    // combined all the images into the final image
    transparentSnapshots.foreach { snapshot =>
      finalImage.getGraphics.drawImage(snapshot, 0, 0, null)
    }
    // save the final image to a file
    javax.imageio.ImageIO.write(finalImage, "png", new java.io.File(path + "/" + name + ".png"))
  }

  override def close(environment: Environment[T, P], time: Time, l: Long): Unit = {}

  override def setup(environment: Environment[T, P]): Unit = {}
}
