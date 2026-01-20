package scala.c.engine.cFunctions

import org.eclipse.cdt.core.dom.ast.*
import org.eclipse.cdt.internal.core.dom.parser.c.*

import java.io.File
import java.util.{Formatter, Locale}
import scala.c.engine.*
import scala.c.engine.models.{Function, *}
import scala.collection.mutable.ListBuffer
import scala.util.Try

object Mathh {

	/////////////////////////////////////////////////////////////////
	//                   <math.h> functions                        //
	/////////////////////////////////////////////////////////////////

	def addFunctions(scalaFunctions: ListBuffer[Function])(implicit theState: State) = {

		scalaFunctions += new OneParameterFunction[Float]("sqrt") {
			def func(num: Float) = {
				Some(Math.sqrt(num))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Double]("fabs") {
			def func(x: Double) = {
				Some(Math.abs(x))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Double]("sin") {
			def func(x: Double) = {
				Some(Math.sin(x))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Double]("cos") {
			def func(x: Double) = {
				Some(Math.cos(x))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Double]("tan") {
			def func(x: Double) = {
				Some(Math.tan(x))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Double]("acos") {
			def func(x: Double) = {
				Some(Math.acos(x))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Double]("cosh") {
			def func(x: Double) = {
				Some(Math.cosh(x))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Double]("asin") {
			def func(x: Double) = {
				Some(Math.asin(x))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Double]("sinh") {
			def func(x: Double) = {
				Some(Math.sinh(x))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Double]("atan") {
			def func(x: Double) = {
				Some(Math.atan(x))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Double]("tanh") {
			def func(x: Double) = {
				Some(Math.tanh(x))
			}
		}.generate

		scalaFunctions += new TwoParameterFunction[Double, Double]("atan2") {
			def func(x: Double, y: Double) = {
				Some(Math.atan2(x, y))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Double]("log10f") {
			def func(x: Double) = {
				Some(Math.log10(x))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Double]("exp") {
			def func(x: Double) = {
				Some(Math.exp(x))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Double]("ceil") {
			def func(x: Double) = {
				Some(Math.ceil(x))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Double]("floor") {
			def func(x: Double) = {
				Some(Math.floor(x))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Double]("log") {
			def func(x: Double) = {
				Some(Math.log(x))
			}
		}.generate

		scalaFunctions += new OneParameterFunction[Double]("log10") {
			def func(x: Double) = {
				Some(Math.log10(x))
			}
		}.generate

		scalaFunctions += new TwoParameterFunction[Double, Double]("pow") {
			def func(base: Double, exp: Double) = {
				Some(Math.pow(base, exp))
			}
		}.generate

		scalaFunctions += new TwoParameterFunction[Double, Double]("fmod") {
			def func(first: Double, second: Double) = {
				Some(first % second)
			}
		}.generate
	}
}
