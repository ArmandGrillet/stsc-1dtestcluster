package uicluster

// UI
import java.awt.{Color, Graphics2D, Paint}
import java.awt.image.BufferedImage
import javafx.scene.{chart => jfxsc}
import org.jfree.chart.axis.{NumberTickUnit, TickUnits}
import scala.collection.mutable.ListBuffer
import scala.util.control._
import scalafx.embed.swing.SwingFXUtils
import scalafx.event.ActionEvent
import scalafx.scene.chart._
import scalafx.scene.control._
import scalafx.scene.control.Alert._
import scalafx.scene.image.ImageView
import scalafx.scene.layout.AnchorPane
import scalafx.scene.Node
import scalafx.stage.FileChooser
import scalafx.stage.FileChooser.ExtensionFilter
import scalafxml.core.{NoDependencyResolver, FXMLLoader}
import scalafxml.core.macros.sfxml

// Logic
import breeze.linalg._
import breeze.stats.distributions.Gaussian
import breeze.plot._
import java.io.File
import stsc.STSC

@sfxml
class Controller(private val root: AnchorPane, private val sigma: TextField, private val observations: TextField, private val distance: TextField, val dataset: ImageView, val clusters: ImageView) {

    private var displayedDataset = DenseMatrix.zeros[Double](0, 0)

    def run(event: ActionEvent) {
        var sigmaInput = toDouble(sigma.text.value).getOrElse(0.0)
        var observationsInput = toInt(observations.text.value).getOrElse(0)
        var distanceInput = toDouble(distance.text.value).getOrElse(0.0)
        var ready = true

        if (ready && sigmaInput < 0) {
            showAlert("Sigma has to be positive", "Sigma has to be positive.")
            ready = false
        }

        if (ready && observationsInput < 10) {
            showAlert("The number of observations has to be more than 10", "The number of observations has to be more than 10.")
            ready = false
        }

        if (ready && distanceInput < 0) {
            showAlert("Distance has to be positive", "The distance between the two dataset has to be positive.")
            ready = false
        }

        if (ready) {
            val f1 = Figure()
            f1.visible = false
            f1.width = dataset.getBoundsInParent().getWidth().toInt
            f1.height = dataset.getBoundsInParent().getHeight().toInt
            val f2 = Figure()
            f2.visible = false
            f2.width = dataset.getBoundsInParent().getWidth().toInt
            f2.height = dataset.getBoundsInParent().getHeight().toInt


            val p1 = f1.subplot(0)
            p1.title = "Dataset"

            val sample1 = Gaussian(0, sigmaInput).sample(observationsInput)
            val sample2 = Gaussian(distanceInput, sigmaInput).sample(observationsInput)

            p1 += breeze.plot.hist(sample1, 100)
            p1 += breeze.plot.hist(sample2, 100)

            dataset.image = SwingFXUtils.toFXImage(imageToFigure(f1), null)

            val samplesVector = DenseVector((sample1 ++ sample2).toArray)
            val samplesMatrix = DenseMatrix.zeros[Double](observationsInput * 2, 1)
            samplesMatrix(::, 0) := samplesVector
            val (cBest, _, clusts) = STSC.cluster(samplesMatrix)

            var displayableResult = new Array[ListBuffer[Double]](6)
            displayableResult = displayableResult.map(r => new ListBuffer[Double])
            var i = 0
            for (i <- 0 until clusts.length){
                displayableResult(clusts(i)) += samplesVector(i)
            }

            for (i <- 0 until cBest) {
                if (displayableResult(i).length > 0) {
                    val histogram = f2.subplot(cBest, 1, i)
                    histogram.xlim = p1.xlim
                    histogram += breeze.plot.hist(displayableResult(i), 100)
                }
            }

            clusters.image = SwingFXUtils.toFXImage(imageToFigure(f2), null)
        }
    }

    private def imageToFigure(f: Figure): BufferedImage = {
        val image = new BufferedImage(f.width, f.height, BufferedImage.TYPE_INT_ARGB)
        val g2d = image.createGraphics()
        f.drawPlots(g2d)
        g2d.dispose
        return image
    }

    private def showAlert(header: String, content: String) {
        new Alert(AlertType.Error) {
            title = "Error"
            headerText = header
            contentText = content
        }.showAndWait()
    }

    private def showDataset() {
        // val f = Figure()
        // f.visible = false
        // f.width = dataset.getBoundsInParent().getWidth().toInt
        // f.height = dataset.getBoundsInParent().getHeight().toInt
        // val p = f.subplot(0)
        // p.title = "Dataset"
        // p += scatter(displayedDataset(::, 0), displayedDataset(::, 1), {(_:Int) => 0.01}, {(pos:Int) => Color.BLACK}) // Display the observations.
        // dataset.image = SwingFXUtils.toFXImage(imageToFigure(f), null)
        // qualities.text = ""
    }

    private def toDouble(s: String): Option[Double] = {
        try {
            Some(s.toDouble)
        } catch {
            case e: Exception => None
        }
    }

    private def toInt(s: String): Option[Int] = {
        try {
            Some(s.toInt)
        } catch {
            case e: Exception => None
        }
    }
}
