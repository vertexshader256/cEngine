package scala.c.engine

import org.eclipse.cdt.core.dom.ast.{IASTLiteralExpression, IASTNode, IBasicType}
import org.eclipse.cdt.internal.core.dom.parser.c.*

import scala.collection.mutable.ListBuffer
import scala.io.Source

object CEngine {
	private def callMain(state: State, arguments: List[String]) = {
		state.parseGlobals(state.sources)

		val program = state.context

		val args = List(".") ++ arguments

		val functionCall = if (args.nonEmpty) {
			val fcnName = new CASTIdExpression(new CASTName("main".toCharArray))
			val factory = state.sources.head.getTranslationUnit.getASTNodeFactory
			val sizeExpr = factory.newLiteralExpression(IASTLiteralExpression.lk_integer_constant, args.size.toString)

			val stringType = new CPointerType(new CBasicType(IBasicType.Kind.eChar, IBasicType.IS_UNSIGNED), 0)

			val stringAddresses = args.map { arg =>
				val addr = state.getString("\"" + arg + "\"").value
				RValue(addr, stringType)
			}

			val theType = new CPointerType(stringType, 0)
			val newVar = program.addVariable("mainInfo", theType)
			val start = state.allocateSpace(stringAddresses.size * 4)
			state.writeDataBlock(stringAddresses, start)(state)
			newVar.setValue(RValue(start, TypeHelper.intType))

			val varExpr = factory.newIdExpression(factory.newName("mainInfo"))

			new CASTFunctionCallExpression(fcnName, List(sizeExpr, varExpr).toArray)
		} else {
			null
		}

		state.callTheFunction("main", functionCall, Some(program))(state)
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
		if (!stdout.isEmpty) {
			val results = new ListBuffer[String]()

			var currentString = new ListBuffer[Char]()
			var writeLast = false

			var index = 0
			while (index < stdout.size) {

				if (stdout(index) == '\r') {
					results += currentString.mkString
					currentString = new ListBuffer[Char]()
					writeLast = false
					index += 1
				} else if (stdout(index) == '\n') {
					results += currentString.mkString
					currentString = new ListBuffer[Char]()
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
				val ast = State.parseCode(codeInFiles, includePaths)
				val state = new State(ast, pointerSize)
				state.addMain(ast)
				state
			} else {
				val eePrint = Source.fromFile("./src/scala/c/engine/ee_printf.c", "utf-8").mkString
				val code = Seq("#define HAS_FLOAT\n" + eePrint) ++ codeInFiles.map { code => "#define printf ee_printf \n" + code }
				val ast = State.parseCode(code, includePaths)
				val state = new State(ast, pointerSize)
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
			case e => e.printStackTrace(); List()
		}
	}
}
