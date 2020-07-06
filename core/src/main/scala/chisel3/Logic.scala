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

/** Abstract base class for Logic, which only contains basic combinational blocks
  * These may contain logic which are written in the Module
  * body (constructor).
  * This abstract base class includes an implicit clock and reset.
  *
  * @note Logic instantiations must be wrapped in a Logic() call.
  */
abstract class Logic(implicit moduleCompileOptions: CompileOptions)
    extends RawModule {
  // Implicit clock and reset pins
  final val clock: Clock = IO(Input(Clock()))
  final val reset: Reset = IO(Input(mkReset))

  private[chisel3] def mkReset: Reset = {
    // Top module and compatibility mode use Bool for reset
    val inferReset = _parent.isDefined && moduleCompileOptions.inferModuleReset
    if (inferReset) Reset() else Bool()
  }

  // Setup ClockAndReset
  Builder.currentClock = Some(clock)
  Builder.currentReset = Some(reset)

  private[chisel3] override def initializeInParent(parentCompileOptions: CompileOptions): Unit = {
    implicit val sourceInfo = UnlocatableSourceInfo

    super.initializeInParent(parentCompileOptions)
    clock := Builder.forcedClock
    reset := Builder.forcedReset
  }

  private[chisel3] override def generateComponent(): Component = { // scalastyle:ignore cyclomatic.complexity
    require(!_closed, "Can't generate module more than once")
    _closed = true

    val names = nameIds(classOf[RawModule])

    // Ports get first naming priority, since they are part of a Module's IO spec
    namePorts(names)

    // Then everything else gets named
    for ((node, name) <- names) {
      node.suggestName(name)
    }

    // All suggestions are in, force names to every node.
    for (id <- getIds) {
      id match {
        case id: BaseModule => id.forceName(default=id.desiredName, _namespace)
        case id: MemBase[_] => id.forceName(default="_T", _namespace)
        case id: Data  =>
          if (id.isSynthesizable) {
            id.topBinding match {
              case OpBinding(_) | MemoryPortBinding(_) | PortBinding(_) | RegBinding(_) | WireBinding(_) =>
                id.forceName(default="_T", _namespace)
              case _ =>  // don't name literals
            }
          } // else, don't name unbound types
      }
      id._onModuleClose
    }

    val firrtlPorts = getModulePorts map { port: Data =>
      // Special case Vec to make FIRRTL emit the direction of its
      // element.
      // Just taking the Vec's specifiedDirection is a bug in cases like
      // Vec(Flipped()), since the Vec's specifiedDirection is
      // Unspecified.
      val direction = port match {
        case v: Vec[_] => v.specifiedDirection match {
          case SpecifiedDirection.Input => SpecifiedDirection.Input
          case SpecifiedDirection.Output => SpecifiedDirection.Output
          case SpecifiedDirection.Flip => SpecifiedDirection.flip(v.sample_element.specifiedDirection)
          case SpecifiedDirection.Unspecified => v.sample_element.specifiedDirection
        }
        case _ => port.specifiedDirection
      }

      Port(port, direction)
    }
    _firrtlPorts = Some(firrtlPorts)

    // Generate IO invalidation commands to initialize outputs as unused,
    //  unless the client wants explicit control over their generation.
    val invalidateCommands = {
      if (!compileOptions.explicitInvalidate) {
        getModulePorts map { port => DefInvalid(UnlocatableSourceInfo, port.ref) }
      } else {
        Seq()
      }
    }

    // Logic instance does not contain any registers
    for(command <- getCommands){
        command match{
            case reg: DefReg => throwException("Logic class cannot contain Reg")
            case regInit: DefRegInit => throwException("Logic class cannot contain RegInit")
            case defMemory: DefMemory => throwException("Logic class cannot contain Memory")
            case defSeqMemory: DefSeqMemory => throwException("Logic class cannot contain SeqMemory")
            case defMemPort: DefMemPort[t] => throwException("Logic class cannot contain Memport") 
            case _ => ()
        }
    }

    val component = DefModule(this, name, firrtlPorts, invalidateCommands ++ getCommands)
    _component = Some(component)
    component
  }
}

object Logic extends SourceInfoDoc {
  /** A wrapper method that all Logic instantiations must be wrapped in
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
                                         compileOptions: CompileOptions): T = {
    if (Builder.readyForModuleConstr) {
      throwException("Error: Called Module() twice without instantiating a Module." +
                     sourceInfo.makeMessage(" See " + _))
    }
    Builder.readyForModuleConstr = true

    val parent = Builder.currentModule
    val whenDepth: Int = Builder.whenDepth

    // Save then clear clock and reset to prevent leaking scope, must be set again in the Module
    val (saveClock, saveReset)  = (Builder.currentClock, Builder.currentReset)
    Builder.currentClock = None
    Builder.currentReset = None

    // Execute the module, this has the following side effects:
    //   - set currentModule
    //   - unset readyForModuleConstr
    //   - reset whenDepth to 0
    //   - set currentClockAndReset
    val module: T = bc  // bc is actually evaluated here

    // We want to check whether this is logic or not
    module match{
        case logic: Logic => ()
        case _ => throwException("Only subclass of Logic type can be instantiated in Logic wrapper")
    }

    if (Builder.whenDepth != 0) {
      throwException("Internal Error! when() scope depth is != 0, this should have been caught!")
    }
    if (Builder.readyForModuleConstr) {
      throwException("Error: attempted to instantiate a Module, but nothing happened. " +
                     "This is probably due to rewrapping a Module instance with Module()." +
                     sourceInfo.makeMessage(" See " + _))
    }
    Builder.currentModule = parent // Back to parent!
    Builder.whenDepth = whenDepth
    Builder.currentClock = saveClock   // Back to clock and reset scope
    Builder.currentReset = saveReset

    val component = module.generateComponent()
    Builder.components += component

    // Handle connections at enclosing scope
    if(!Builder.currentModule.isEmpty) {
      pushCommand(DefInstance(sourceInfo, module, component.ports))
      module.initializeInParent(compileOptions)
    }
    module
  }

  /** Returns the implicit Clock */
  def clock: Clock = Builder.forcedClock
  /** Returns the implicit Reset */
  def reset: Reset = Builder.forcedReset
  /** Returns the current Module */
  def currentModule: Option[BaseModule] = Builder.currentModule
}