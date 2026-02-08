package scala.c.engine

import org.scalatest.*
import org.scalatest.flatspec.AsyncFlatSpec

import java.util.concurrent.atomic.AtomicInteger
import scala.c.engine.models.NumBits
import scala.c.engine.models.NumBits.*
import scala.c.engine.gcc.Gcc
import scala.concurrent.*

object StandardTest {
	val cFileCount = new AtomicInteger()
	val exeCount = new AtomicInteger()

	def getGccOutput(codeInFiles: Seq[String], pointerSize: NumBits = SixtyFourBits,
													 args: List[String] = List(), includePaths: List[String] = List()): Seq[String] = {
		TestResults.loadSavedResults()

		val ptrSize = pointerSize match {
			case ThirtyTwoBits => 32
			case SixtyFourBits => 64
		}

		val codeBeingRun = codeInFiles.mkString + args.mkString + ptrSize.toString

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

	def checkResults(code: String, shouldBootstrap: Boolean = true, pointerSize: NumBits = SixtyFourBits,
									 args: List[String] = List(), includePaths: List[String] = List(), runConcurrent: Boolean = true) = {
		testGccVsCEngine(Seq(code), shouldBootstrap, pointerSize, args, includePaths, runConcurrent)
	}

	def testGccVsCEngine(codeInFiles: Seq[String], shouldBootstrap: Boolean = true, pointerSize: NumBits = SixtyFourBits,
										args: List[String] = List(), includePaths: List[String] = List(), runConcurrent: Boolean = true): Future[Assertion] = {

		if (runConcurrent) {
			val gccResults = Future {
				StandardTest.getGccOutput(codeInFiles, pointerSize, args, includePaths)
			}

			val cEngineResults = Future {
				Results.getCEngineOutput(codeInFiles, shouldBootstrap, pointerSize, args, includePaths)
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
				val cEngineOutput = Results.getCEngineOutput(codeInFiles, shouldBootstrap, pointerSize, args, includePaths)
				info("C_Engine output: " + cEngineOutput)
				info("Gcc      output: " + gccOutput)

				assert(cEngineOutput === gccOutput)
			}
		}
	}
}