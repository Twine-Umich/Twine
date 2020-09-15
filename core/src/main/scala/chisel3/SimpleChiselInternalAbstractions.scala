package chisel3.simplechisel

import scala.collection.JavaConversions._
import scala.language.experimental.macros
import chisel3._
import chisel3.experimental._
import chisel3.internal._
import chisel3.internal.Builder._
import chisel3.internal.firrtl._
import chisel3.internal.sourceinfo._
import scala.collection.mutable.{ListBuffer, HashMap, ArrayBuffer, Set}


/** Abstract base class for SimpleChiselState that contain Chisel RTL.
  * This abstract base class is a user-defined module which does not include implicit clock and reset and supports
  * multiple IO() declarations.
  */
abstract class SimpleChiselModuleInternal(implicit moduleCompileOptions: CompileOptions) 
    extends MultiIOModule {

      // IO for this Module. At the Scala level (pre-FIRRTL transformations),
      // connections in and out of a Module may only go through `in and out` elements.
      def in: Record
      def out: Record
      def ctrl: Record

      val sub_modules: Set[SimpleChiselModuleInternal] = Set()

      def >>>(that: Aggregate): Aggregate
      def >>>[T <: SimpleChiselModuleInternal](that: T): T

      private[chisel3] def generateSimpleChiselComponent: Any
      // These are to be phased out
      protected var override_clock: Option[Clock] = None
      protected var override_reset: Option[Bool] = None

      // Allow access to bindings from the compatibility package
      // protected def _compatIoPortBound() = portsContains(io)// scalastyle:ignore method.name

      private[chisel3] override def namePorts(names: HashMap[HasId, String]): Unit = {
        for (port <- getModulePorts) {
          // This should already have been caught
          if (!names.contains(port)) throwException(s"Unable to name port $port in $this")
          val name = names(port)
          port.setRef(ModuleIO(this, _namespace.name(name)))
        }
      }

      private[chisel3] override def generateComponent(): Component = {
        _compatAutoWrapPorts()  // pre-IO(...) compatibility hack

        require((portsContains(clock)) && (portsContains(reset)), "Internal error, module did not have clock or reset as IO") // scalastyle:ignore line.size.limit

        super.generateComponent()
      }

      private[chisel3] override def initializeInParent(parentCompileOptions: CompileOptions): Unit = {
        // Don't generate source info referencing parents inside a module, since this interferes with
        // module de-duplication in FIRRTL emission.
        implicit val sourceInfo = UnlocatableSourceInfo

        // if (!parentCompileOptions.explicitInvalidate) {
        //   pushCommand(DefInvalid(sourceInfo, io.ref))
        // }

        clock := override_clock.getOrElse(Builder.forcedClock)
        reset := override_reset.getOrElse(Builder.forcedReset)
      }
    }


trait ValidInterfaceInternal{
    def valid: Bool
}

trait DecoupledInterfaceInternal extends ValidInterfaceInternal{
    def ready: Bool
}

trait OutOfOrderInterfaceInternal extends DecoupledInterfaceInternal{
    def ticket_num: UInt
}

trait SimpleChiselIOCtrlInternal{
  def in: ValidInterfaceInternal
  def out: ValidInterfaceInternal
}

trait NoIOCtrlInternal{}

trait LockStepIOCtrlInternal extends SimpleChiselIOCtrlInternal{
  def stall: Bool
  def stuck: Bool  
}
trait TightlyCoupledIOCtrlInternal extends LockStepIOCtrlInternal{
    def delay: Int
}

trait ValidIOCtrlInternal extends LockStepIOCtrlInternal{}

trait LatInsensitiveIOCtrlInternal extends SimpleChiselIOCtrlInternal{
    override def in: DecoupledInterfaceInternal
    override def out: DecoupledInterfaceInternal
}
trait DecoupledIOCtrlInternal extends LatInsensitiveIOCtrlInternal{
    def size_of_receiving_buffer: Int
    def size_of_sending_buffer: Int
}

trait OutOfOrderIOCtrlInternal extends LatInsensitiveIOCtrlInternal{
  def size_of_reorder_buffer: Int
  override def in: OutOfOrderInterfaceInternal
  override def out: OutOfOrderInterfaceInternal
}
