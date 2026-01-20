package scala.c.engine.cFunctions

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.*

import java.io.File
import java.util.{Formatter, Locale}
import scala.c.engine.*
import scala.c.engine.models.{Function, *}
import scala.collection.mutable.ListBuffer
import scala.util.Try

object Stdargh {

	/////////////////////////////////////////////////////////////////
	//                  <stdarg.h> functions                       //
	/////////////////////////////////////////////////////////////////

	def addFunctions(scalaFunctions: ListBuffer[Function]) = {

		scalaFunctions += new EmulatedFunction("va_arg") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val argTypeStr = formattedOutputParams(0).value.asInstanceOf[Int]

				val str = Utils.readString(Address(argTypeStr))(using state)

				val (offset, theType) = str match {
					case "unsigned int" => (4, TypeHelper.unsignedIntType)
					case "int" => (4, TypeHelper.intType)
					case "double" => (8, TypeHelper.doubleType)
					case "char" => (1, TypeHelper.charType)
					case "char *" => (4, CPointerType(TypeHelper.charType, 0))
					case "unsigned long" => (8, CPointerType(CBasicType(IBasicType.Kind.eInt, IBasicType.IS_LONG), 0))
				}

				val current = state.varArgStartingAddr.head
				state.varArgStartingAddr = state.varArgStartingAddr.tail
				val result = state.stack.readFromMemory(Address(current), theType).value
				state.varArgStartingAddr = (current + offset) +: state.varArgStartingAddr

				Some(RValue(result))
			}
		}

		scalaFunctions += new EmulatedFunction("va_start") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				val lastNamedArgAddr = formattedOutputParams(0).value.asInstanceOf[Int]
				state.varArgStartingAddr = (lastNamedArgAddr + 4) +: state.varArgStartingAddr
				None
			}
		}

		scalaFunctions += new EmulatedFunction("va_end") {
			def run(formattedOutputParams: Array[RValue], state: State): Option[RValue] = {
				state.varArgStartingAddr = state.varArgStartingAddr.tail
				None
			}
		}
	}
}
