package scala.c.engine

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.collection.mutable
import upickle.default.*

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}

object TestResults {
	val digest = MessageDigest.getInstance("SHA-256")
	val results: mutable.Map[String, String] = mutable.HashMap()
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

	def addResult(test: String, result: String) = {
		val key = getHash(test)
		results.put(key, result)
	}

	def getSavedResult(test: String): Option[String] = {
		val key = getHash(test)
		results.get(key)
	}

	def loadSavedResults() = {
		val resultsBytes = Files.readAllBytes(Paths.get(resultsFileName))
		results ++= read[Map[String, String]](resultsBytes)
	}

	def writeResultsFile() = {
		val jsonString: String = write(results)
		val writer = new PrintWriter(new File(resultsFileName))
		writer.write(jsonString)
	}
}
