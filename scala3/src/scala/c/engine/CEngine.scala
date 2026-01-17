package scala.c.engine

import org.eclipse.cdt.core.dom.ast.{IASTLiteralExpression, IASTNode, IBasicType}
import org.eclipse.cdt.internal.core.dom.parser.c.*

import scala.c.engine.models.*
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Using

object CEngine {
	private def callMain(state: State, arguments: List[String]) = {
		state.parseGlobals(state.sources)

		val program = state.context

		val args = List(".") ++ arguments

		val functionCall = if (args.nonEmpty) {
			val fcnName = CASTIdExpression(CASTName("main".toCharArray))
			val factory = state.sources.head.getTranslationUnit.getASTNodeFactory
			val numberOfArgsLit = factory.newLiteralExpression(IASTLiteralExpression.lk_integer_constant, args.size.toString)

			val stringType = CPointerType(CBasicType(IBasicType.Kind.eChar, IBasicType.IS_UNSIGNED), 0)

			// convert main args to strings, write them to memory, get the addresses
			val stringValues = args.map { arg =>
				val addr = state.getString("\"" + arg + "\"").value
				RValue(addr, stringType)
			}

			// create a pointer to the strings
			val stringPointer = program.addVariable("argStringPtr", CPointerType(stringType, 0))

			val start = state.allocateStack(stringValues.size * state.pointerSize.ptrSize) // 4 bytes per pointer
			state.writeValues(start, stringValues)
			stringPointer.setValue(RValue(start.location, TypeHelper.intType))

			val argStringPtrId = factory.newIdExpression(factory.newName("argStringPtr"))

			CASTFunctionCallExpression(fcnName, List(numberOfArgsLit, argStringPtrId).toArray)
		} else {
			null
		}

		state.callTheFunction("main", functionCall, Some(program))
	}

	private def getErrors(node: IASTNode, errors: List[String]): List[String] = {
		node match {
			case prob: CASTProblemDeclaration =>
				println("ERROR: " + prob.getProblem.getRawSignature)
				List("Error on: " + prob.getFileLocation.getFileName + ".c:" + prob.getFileLocation.getStartingLineNumber + ":" + prob.getParent.getRawSignature)
			case _ => errors ++ node.getChildren.toList.flatMap { x => getErrors(x, errors) }
		}
	}

	def getResults(stdout: List[Char]): List[String] = {
		if (stdout.nonEmpty) {
			val results = ListBuffer[String]()

			var currentString = ListBuffer[Char]()
			var writeLast = false

			var index = 0
			while (index < stdout.size) {

				if (stdout(index) == '\r') {
					results += currentString.mkString
					currentString = ListBuffer[Char]()
					writeLast = false
					index += 1
				} else if (stdout(index) == '\n') {
					results += currentString.mkString
					currentString = ListBuffer[Char]()
					writeLast = false
					index += 1
				} else {
					currentString += stdout(index)
					writeLast = true
					index += 1
				}
			}

			if (writeLast) {
				results += currentString.mkString
			}
			results.toList
		} else {
			List()
		}
	}

	def getCEngineOutput(codeInFiles: Seq[String], shouldBootstrap: Boolean, pointerSize: NumBits,
															 arguments: List[String], includePaths: List[String]): List[String] = {
		try {

			val state = if (shouldBootstrap) {
				val ast = Utils.getTranslationUnits(codeInFiles, includePaths)
				val state = State(ast, pointerSize)
				state.addMain(ast)
				state
			} else {
				val eePrint = Using(Source.fromFile("./src/scala/c/engine/cFunctions/ee_printf.c", "utf-8")) { source =>
					source.mkString
				}.get
				val code = Seq("#define HAS_FLOAT\n" + eePrint) ++ codeInFiles.map { code => "#define printf ee_printf \n" + code }
				val ast = Utils.getTranslationUnits(code, includePaths)
				val state = State(ast, pointerSize)
				state.addMain(ast)
				state
			}

			val errors = state.sources.flatMap { tUnit => getErrors(tUnit, List()) }

			if errors.isEmpty then
				// Good to go!
				callMain(state, arguments)
				getResults(state.stdout.toList)
			else
				errors
		} catch {
			case e: Throwable => e.printStackTrace(); List()
		}
	}
}
