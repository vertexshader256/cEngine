package scala.c.engine

import org.scalatest.*
import org.scalatest.flatspec.AsyncFlatSpec

import java.util.concurrent.atomic.AtomicInteger
import scala.c.engine.models.NumBits
import scala.c.engine.models.NumBits.*
import scala.concurrent.*

object StandardTest {
	val cFileCount = new AtomicInteger()
	val exeCount = new AtomicInteger()

	private def getGccOutput(codeInFiles: Seq[String], pointerSize: NumBits = ThirtyTwoBits,
													 args: List[String] = List(), includePaths: List[String] = List()): Seq[String] = {
		TestResults.loadSavedResults()

		val codeBeingRun = codeInFiles.mkString

		TestResults.getSavedGccOutput(codeBeingRun).map: priorRunResult =>
			priorRunResult
		.getOrElse:
			val testId = StandardTest.exeCount.incrementAndGet.toString
			val gccOutput = Gcc.getGccOutput(codeInFiles, testId, pointerSize, args, includePaths)

			if gccOutput.wasSuccess then // only cache results if gcc was successful ran
				TestResults.addGccResult(codeBeingRun, gccOutput.output)
				TestResults.writeResultsFile()

			gccOutput.output
	}
}

abstract class StandardTest2(name: String = "", code: String) extends StandardTest {
	val numBits: NumBits = SixtyFourBits

	name should "print the correct results" in {
		checkResults(code, pointerSize = numBits)
	}
}

class StandardTest extends AsyncFlatSpec {
	implicit override def executionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

	def checkResults(code: String, shouldBootstrap: Boolean = true, pointerSize: NumBits = ThirtyTwoBits,
									 args: List[String] = List(), includePaths: List[String] = List(), runConcurrent: Boolean = true) = {
		testGccVsCEngine(Seq(code), shouldBootstrap, pointerSize, args, includePaths, runConcurrent)
	}

	def testGccVsCEngine(codeInFiles: Seq[String], shouldBootstrap: Boolean = true, pointerSize: NumBits = ThirtyTwoBits,
										args: List[String] = List(), includePaths: List[String] = List(), runConcurrent: Boolean = true): Future[Assertion] = {

		if (runConcurrent) {
			val gccResults = Future {
				StandardTest.getGccOutput(codeInFiles, pointerSize, args, includePaths)
			}

			val cEngineResults = Future {
				CEngine.getCEngineOutput(codeInFiles, shouldBootstrap, pointerSize, args, includePaths)
			}

			for {
				gccOutput <- gccResults
				cEngineOutput <- cEngineResults
			} yield {
				info("C_Engine output: " + cEngineOutput)
				info("Gcc      output: " + gccOutput)

				assert(cEngineOutput === gccOutput)
			}
		} else {
			Future {
				val gccOutput = StandardTest.getGccOutput(codeInFiles, pointerSize, args, includePaths)
				val cEngineOutput = CEngine.getCEngineOutput(codeInFiles, shouldBootstrap, pointerSize, args, includePaths)
				info("C_Engine output: " + cEngineOutput)
				info("Gcc      output: " + gccOutput)

				assert(cEngineOutput === gccOutput)
			}
		}
	}
}