package app.astViewer

import org.eclipse.cdt.core.dom.ast.IASTFunctionCallExpression

import org.eclipse.cdt.core.dom.ast._

import scala.collection.mutable.{ListBuffer, Stack}
import scala.util.control.Exception.allCatch
import java.util.Formatter;
import java.util.Locale;
import Functions._
import org.eclipse.cdt.internal.core.dom.parser.c.CBasicType
import org.eclipse.cdt.internal.core.dom.parser.c.CStructure
import scala.collection.immutable.HashMap
import org.eclipse.cdt.internal.core.dom.parser.c.CPointerType

object FunctionCallExpr {
  
  val varArgs = new ListBuffer[Any]()
  
  // resolves 'Any' to 'ValueInfo'
//  def resolve(any: Any)(implicit state: State): ValueInfo = {
//    any match {
//      case Variable(info: Variable) => info.value
//      case lit @ Literal(_) => lit.cast
//      case Address(addy) => ValueInfo(addy, TypeHelper.pointerType)
//      case AddressInfo(addy, theType) =>
//          state.readVal(addy, TypeHelper.resolve(theType))
//      case value @ ValueInfo(theVal, _) => value
//      case int: Int => ValueInfo(int, null)
//      case float: Float => ValueInfo(float, null)
//      case double: Double => ValueInfo(double, null)
//      case long: Long => ValueInfo(long, null)
//      case boolean: Boolean => ValueInfo(boolean, null)
//      case byte: Byte => ValueInfo(byte, null)
//    }
//  }
  
  def formatArgument(arg: Any)(implicit state: State): AnyVal = {
    arg match {  
        case addr @ Address(address) => addr
        case AddressInfo(address, theType) => 
          state.readVal(address, TypeHelper.resolve(theType)).value
        
        case int: Int => int
        case float: Float => float
        case short: Short => short
        case long: Long => long
        case doub: Double => doub
        case char: Character => char
        case lit @ Literal(_) => lit.cast.value
      }
  }
  
  def allocateString(arg: Any)(implicit state: State): AnyVal = {
    arg match {
        case Variable(theInfo: Variable) => 
          val info = theInfo.info
          if (TypeHelper.isPointer(info.theType) && TypeHelper.getPointedType(info.theType).isInstanceOf[IBasicType] &&
              TypeHelper.getPointedType(info.theType).asInstanceOf[IBasicType].getKind == IBasicType.Kind.eChar) {
            Address(state.readVal(info.address, info.theType).value.asInstanceOf[Int])
          } else {
            state.readVal(info.address, info.theType).value
          }
        case ValueInfo(theVal, _) =>             
          theVal
        case StringLiteral(str) => 
          val strAddr = state.createStringVariable(str)
          strAddr
          
        case x => TypeHelper.resolve(x).value
      }
  }
  
  def parse(call: IASTFunctionCallExpression, direction: Direction)(implicit state: State): Seq[IASTNode] = {
    if (direction == Exiting) {
        val name = state.stack.pop match {
          case VarRef(str) => str
          case AddressInfo(addr, theType) => state.getFunctionByIndex(state.readPtrVal(addr)).name
        }

        val fcnDef = if (state.hasFunction(name)) {
          state.getFunction(name)
        } else {
          val theVar = state.functionContexts.head.varMap(name)
          state.getFunctionByIndex(theVar.value.value.asInstanceOf[Int])
        }

        val argStack = new Stack[IType]()
        fcnDef.parameters.reverse.foreach{ arg =>
          argStack.push(arg)
        }
    
        var startingAddress: Address = Address(-1)
        
        // do this up here so string allocation doesnt clobber arg stack
        val results = call.getArguments.map{call => 
          val x = state.stack.pop
          val string = allocateString(x)
          (x, string)
        }

        results.foreach { case (value, result) => 
          
          val (paramType, argVal) = if (!argStack.isEmpty) {  
            val theType = argStack.pop
            (theType, TypeHelper.cast(theType, result).value)
          } else if (value.isInstanceOf[Literal]) {
            val theType = value.asInstanceOf[Literal].cast.theType
            (theType, result)
          } else if (value.isInstanceOf[StringLiteral]) {
            (new CPointerType(new CBasicType(IBasicType.Kind.eChar, 0), 0), result)
          } 
          
          else {
            // 8 bytes should be enough in generic case
            (TypeHelper.qword, result)
          }
          
                    
          val argumentSpace = state.allocateSpace(TypeHelper.sizeof(paramType))
          
          if (startingAddress == Address(-1)) {
            startingAddress = argumentSpace
          }
          
          state.setValue(argVal, argumentSpace)
          
          // hacky: special case for var-args in scala function
          if (name == "printf") {
//            if (value.isInstanceOf[StringLiteral]) {
//              state.stack.push(value.asInstanceOf[StringLiteral])
//            } else {
              state.stack.push(result)
           // }
          }
          
        }

        state.callTheFunction(name, call, startingAddress)

      } else {
        call.getArguments.reverse ++ Seq(call.getFunctionNameExpression)
      }
  }
}