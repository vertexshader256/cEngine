package scala.c.engine

import upickle.default.*

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.security.MessageDigest
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable

object TestResults {
	var areResultsLoaded = false
	val resultCache: ConcurrentHashMap[String, Seq[String]] = new ConcurrentHashMap[String, Seq[String]]()
	val resultsFileName = "results.json"

	private def getHash(key: String): String = {
		val digest = MessageDigest.getInstance("SHA-256")
		val hashBytes = digest.digest(key.getBytes(StandardCharsets.UTF_8))

		// Convert the byte array into a hexadecimal string
		val hexString = StringBuilder()

		hashBytes.foreach { b =>
			val hex = Integer.toHexString(0xff & b)
			if hex.length() == 1 then
				hexString.append('0'); // Pad with a leading zero if the hex value is a single digit

			hexString ++= hex
		}

		hexString.toString
	}

	def addGccResult(test: String, results: Seq[String]) = {
		val key = getHash(test)
		resultCache.put(key, results.toList)
	}

	def getSavedGccOutput(test: String): Option[Seq[String]] = {
		val key = getHash(test)
		Option(resultCache.get(key))
	}

	def loadSavedResults() = {
		val resultsFile = Paths.get(resultsFileName)

		if !areResultsLoaded && resultsFile.toFile.exists() then
			val resultsBytes = Files.readAllBytes(resultsFile)
			val priorResults = read[Map[String, Seq[String]]](resultsBytes)
			priorResults.foreach: result =>
				resultCache.put(result._1, result._2)
			println(s"Loading saved results for ${priorResults.size} tests")
			areResultsLoaded = true
	}

	def writeResultsFile() = {
		import scala.jdk.CollectionConverters.*
		val scalaMap = mutable.Map[String, Seq[String]]()
		resultCache.asScala.foreach: result =>
			scalaMap += result

		val jsonString: String = write(scalaMap)

		val bw = BufferedWriter(FileWriter(File(resultsFileName)))
		bw.write(jsonString)
		bw.close()
	}
}
