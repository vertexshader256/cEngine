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
  
  def allocateString(arg: Any)(implicit state: State): AnyVal = {
    arg match {
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
        
        val results = if (name == "printf") {
          
          // do this up here so string allocation doesnt clobber arg stack
          val results = call.getArguments.map{call => 
            state.stack.pop match {
              case Variable(theInfo: Variable) => 
                val info = theInfo.info
                if (TypeHelper.isPointer(info.theType) && TypeHelper.getPointedType(info.theType).isInstanceOf[IBasicType] &&
                    TypeHelper.getPointedType(info.theType).asInstanceOf[IBasicType].getKind == IBasicType.Kind.eChar) {
                  Address(state.readVal(info.address, info.theType).value.asInstanceOf[Int])
                } else {
                  state.readVal(info.address, info.theType).value
                }
              case x => allocateString(x)
            }
            
          }
          
          results.foreach{ result =>  state.stack.push(result)}
          results
        } else {
          
          // do this up here so string allocation doesnt clobber arg stack
          val results = call.getArguments.map{call => 
            val x = state.stack.pop
            allocateString(x)
          }
          
          results.foreach { result => 
            
            val (paramType, argVal) = if (!argStack.isEmpty) {  
              val theType = argStack.pop
              (theType, TypeHelper.cast(theType, result).value)
            } else {
              (TypeHelper.getType(result), result)
            } 
                      
            val argumentSpace = state.allocateSpace(TypeHelper.sizeof(paramType))
            
            if (startingAddress == Address(-1)) {
              startingAddress = argumentSpace
            }
            
            state.setValue(argVal, argumentSpace)            
          }
          results
        }

        state.callTheFunction(name, call, startingAddress, results)

      } else {
        call.getArguments.reverse ++ Seq(call.getFunctionNameExpression)
      }
  }
}