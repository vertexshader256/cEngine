package scala.c.engine

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.collection.mutable
import upickle.default.*

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}

object TestResults {
	var areResultsLoaded = false
	val digest = MessageDigest.getInstance("SHA-256")
	val resultCache: mutable.Map[String, Seq[String]] = mutable.HashMap()
	val resultsFileName = "results.json"

	private def getHash(key: String): String = {
		val hashBytes = digest.digest(key.getBytes(StandardCharsets.UTF_8))

		// Convert the byte array into a hexadecimal string
		val hexString = new StringBuilder()

		hashBytes.foreach { b =>
			val hex = Integer.toHexString(0xff & b)
			if hex.length() == 1 then
				hexString.append('0'); // Pad with a leading zero if the hex value is a single digit

			hexString ++= hex
		}

		hexString.toString
	}

	def addResult(test: String, results: Seq[String]) = {
		val key = getHash(test)
		resultCache.put(key, results)
	}

	def getSavedResult(test: String): Option[Seq[String]] = {
		val key = getHash(test)
		resultCache.get(key)
	}

	def loadSavedResults() = {
		val resultsFile = Paths.get(resultsFileName)

		if !areResultsLoaded && resultsFile.toFile.exists() then
			val resultsBytes = Files.readAllBytes(resultsFile)
			val priorResults = read[Map[String, Seq[String]]](resultsBytes)
			resultCache ++= priorResults
			println(s"Loading saved results for ${priorResults.size} tests")
			areResultsLoaded = true
	}

	def writeResultsFile() = {
		val jsonString: String = write(resultCache)

		val bw = new BufferedWriter(new FileWriter(new File(resultsFileName)))
		bw.write(jsonString)
		bw.close()
	}
}
