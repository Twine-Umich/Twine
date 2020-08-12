package chisel3.simplechisel

import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.collection.JavaConversions._
import scala.language.experimental.macros
import chisel3._
import chisel3.experimental._
import chisel3.internal._
import chisel3.internal.Builder._
import chisel3.internal.firrtl._
import chisel3.internal.sourceinfo._
import scala.collection.mutable.{ListBuffer, HashMap, ArrayBuffer}

  /** Abstract base class for simpleChiselModule that contain Chisel RTL.
  * This abstract base class is a user-defined module which does not include implicit clock and reset and supports
  * multiple IO() declarations.
  */
trait SimpleChiselModuleTrait{
  // IO for this Module. At the Scala level (pre-FIRRTL transformations),
  // connections in and out of a Module may only go through `in and out` elements.
  def in: Record
  def out: Record
  def ctrl: Record

  val to_modules = new ArrayBuffer[SimpleChiselModuleTrait]
  val from_modules = new ArrayBuffer[SimpleChiselModuleTrait]

  def >>>(that: Aggregate): Aggregate
  def >>>[T <: SimpleChiselModuleTrait](that: T): T
}

/** Abstract base class for SimpleChiselState that contain Chisel RTL.
  * This abstract base class is a user-defined module which does not include implicit clock and reset and supports
  * multiple IO() declarations.
  */
abstract class SimpleChiselModuleInternal(implicit moduleCompileOptions: CompileOptions) 
    extends LegacyModule with SimpleChiselModuleTrait{
      private[chisel3] def generateSimpleChiselComponent: Any
    }

/** Abstract base class for SimpleChiselState that contain Chisel RTL.
  * This abstract base class is a user-defined module which does not include implicit clock and reset and supports
  * multiple IO() declarations.
  */
abstract class SimpleChiselStateInternal(implicit moduleCompileOptions: CompileOptions) 
    extends State with SimpleChiselModuleTrait{
      private[chisel3] def generateSimpleChiselComponent: Any
    }

/** Abstract base class for SimpleChiselLogic that contain Chisel RTL.
  * This abstract base class is a user-defined module which does not include implicit clock and reset and supports
  * multiple IO() declarations.
  */
abstract class SimpleChiselLogicInternal(implicit moduleCompileOptions: CompileOptions) 
    extends Logic with SimpleChiselModuleTrait{
      private[chisel3] def generateSimpleChiselComponent: Any
    }

trait SimpleChiselIOCtrlInternal{
  def clear: Bool
}

trait ValidInterfaceInternal{
    def valid: Bool
}

trait DecoupledInterfaceInternal{
    def valid: Bool
    def ready: Bool
}

trait OutOfOrderInterfaceInternal{
    def valid: Bool
    def ready: Bool
    def ticket_num: UInt
}

abstract trait NoIOCtrlInternal{}

abstract trait TightlyCoupledIOCtrlInternal extends SimpleChiselIOCtrlInternal{
    def num_of_cycles: Int
    def stall: Bool
    def stuck: Bool  
    def valid_input: Bool
    def valid_output: Bool
}

abstract trait ValidIOCtrlInternal extends SimpleChiselIOCtrlInternal{
    def in: ValidInterfaceInternal
    def out: ValidInterfaceInternal
    def stall: Bool
    def stuck: Bool
}

abstract trait DecoupledIOCtrlInternal extends SimpleChiselIOCtrlInternal{
    def size_of_receiving_buffer: Int
    def size_of_sending_buffer: Int
    def in: DecoupledInterfaceInternal
    def out: DecoupledInterfaceInternal
}

abstract trait OutOfOrderIOCtrlInternal extends SimpleChiselIOCtrlInternal{
  def size_of_reorder_buffer: Int
  def in: OutOfOrderInterfaceInternal
  def out: OutOfOrderInterfaceInternal
}
