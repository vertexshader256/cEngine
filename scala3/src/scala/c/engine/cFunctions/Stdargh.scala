package scala.c.engine.cFunctions

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.*

import java.io.File
import java.util.{Formatter, Locale}
import scala.c.engine.*
import scala.c.engine.cFunctions.Functions.varArgStartingAddr
import scala.c.engine.models.*
import scala.collection.mutable.ListBuffer
import scala.util.Try

object Stdargh {

	def addFunctions(scalaFunctions: ListBuffer[Function]) = {
		/////////////////////////////////////////////////////////////////
		//                  <stdarg.h> functions                       //
		/////////////////////////////////////////////////////////////////

		scalaFunctions += new EmulatedFunction("va_arg") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val argTypeStr = formattedOutputParams(0).value.asInstanceOf[Int]

				val str = Utils.readString(argTypeStr)(using state)

				val (offset, theType) = str match {
					case "unsigned int" => (4, TypeHelper.unsignedIntType)
					case "int" => (4, TypeHelper.intType)
					case "double" => (8, TypeHelper.doubleType)
					case "char" => (1, TypeHelper.charType)
					case "char *" => (4, CPointerType(TypeHelper.charType, 0))
					case "unsigned long" => (8, CPointerType(CBasicType(IBasicType.Kind.eInt, IBasicType.IS_LONG), 0))
				}

				val current = varArgStartingAddr.head
				varArgStartingAddr = varArgStartingAddr.tail
				val result = state.Stack.readFromMemory(current, theType).value
				varArgStartingAddr = (current + offset) +: varArgStartingAddr

				Some(RValue(result))
			}
		}

		scalaFunctions += new EmulatedFunction("va_start") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val lastNamedArgAddr = formattedOutputParams(0).value.asInstanceOf[Int]
				varArgStartingAddr = (lastNamedArgAddr + 4) +: varArgStartingAddr
				None
			}
		}

		scalaFunctions += new EmulatedFunction("va_end") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				varArgStartingAddr = varArgStartingAddr.tail
				None
			}
		}
	}
}
