/* Version 0.1 Last updated: 6/30/2020 */
package chisel3

import scala.collection.immutable.ListMap
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.collection.JavaConversions._
import scala.language.experimental.macros

import java.util.IdentityHashMap
import chisel3.internal._
import chisel3.internal.Builder._
import chisel3.internal.firrtl._
import chisel3.internal.sourceinfo.{InstTransform, SourceInfo}
import chisel3.experimental.BaseModule
import _root_.firrtl.annotations.{ModuleName, ModuleTarget, IsModule}
import chisel3.internal.sourceinfo.UnlocatableSourceInfo
import chisel3.simplechisel._

/** Abstract base class for State, which only contains basic sequential blocks.
  * These may contain both logic and state which are written in the Module
  * body (constructor).
  * This abstract base class includes an implicit clock and reset.
  *
  * @note State instantiations must be wrapped in a State() call.
  */
abstract class State(implicit moduleCompileOptions: CompileOptions)
    extends MultiIOModule {

  private[chisel3] override def generateComponent(): Component = { // scalastyle:ignore cyclomatic.complexity
    val component = super.generateComponent

    var containsReg = false
    // State instance must contain registers
    for(command <- getCommands){
        command match{
            case reg: DefReg => (containsReg = true)
            case regInit: DefRegInit => (containsReg = true)
            case _ => ()
        }
    }
    if(!containsReg){
        throwException("State class must contain Reg/RegNext/RegInit")
    }

    component
  }
}

object State extends SourceInfoDoc {
  /** A wrapper method that all State instantiations must be wrapped in
    * (necessary to help Chisel track internal state).
    *
    * @param bc the Module being created
    *
    * @return the input module `m` with Chisel metadata properly set
    */
  def apply[T <: BaseModule](bc: => T): T = macro InstTransform.apply[T]

  /** @group SourceInfoTransformMacro */
  def do_apply[T <: BaseModule](bc: => T)
                               (implicit sourceInfo: SourceInfo,
                                compileOptions: CompileOptions): T =  Module.do_apply(bc)(sourceInfo,compileOptions)
}