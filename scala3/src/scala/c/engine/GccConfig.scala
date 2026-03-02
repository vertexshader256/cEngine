package scala.c.engine

import java.io.File

object GccConfig {
	val rootDir = raw"C:\msys64\\ucrt64"
	val mainPath = raw"."

	val minGWIncludes = s"$rootDir\\include"
	val minGWAdditionalIncludes: String = File(s"$rootDir\\lib\\gcc\\x86_64-w64-mingw32\\15.2.0\\include").getAbsolutePath
	val minGWMoreIncludes = s"$rootDir\\include\\GL"
}
