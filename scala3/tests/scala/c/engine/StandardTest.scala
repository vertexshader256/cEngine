package scala.c.engine

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.*
import org.scalatest.*
import org.scalatest.flatspec.AsyncFlatSpec

import java.io.{File, PrintWriter}
import java.util.concurrent.atomic.AtomicInteger
import scala.c.engine.NumBits.*
import scala.collection.mutable.ListBuffer
import scala.concurrent.*
import scala.concurrent.duration.Duration
import scala.io.Source
import scala.sys.process.Process

object StandardTest {
	val cFileCount = new AtomicInteger()
	val exeCount = new AtomicInteger()

	private def getGccOutput(codeInFiles: Seq[String], pointerSize: NumBits = ThirtyTwoBits,
													 args: List[String] = List(), includePaths: List[String] = List()) = {
		TestResults.loadSavedResults()

		val codeBeingRun = codeInFiles.mkString

		TestResults.getSavedGccOutput(codeBeingRun).map: priorRunResult =>
			priorRunResult
		.getOrElse:
			val testId = StandardTest.exeCount.incrementAndGet.toString
			val gccOutput = Gcc.getGccOutput(codeInFiles, testId, pointerSize, args, includePaths)
			TestResults.addGccResult(codeBeingRun, gccOutput)
			TestResults.writeResultsFile()
			gccOutput
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
									 args: List[String] = List(), includePaths: List[String] = List()) = {
		testGccVsCEngine(Seq(code), shouldBootstrap, pointerSize, args, includePaths)
	}

	
	
	def testGccVsCEngine(codeInFiles: Seq[String], shouldBootstrap: Boolean = true, pointerSize: NumBits = ThirtyTwoBits,
										args: List[String] = List(), includePaths: List[String] = List()) = {

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
	}
}