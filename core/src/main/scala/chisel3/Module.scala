// See LICENSE for license details.

package chisel3

import collection.mutable.Queue

import scala.collection.immutable.ListMap
import scala.collection.mutable.{ListBuffer, HashMap, StringBuilder, ArrayBuffer}
import scala.collection.JavaConversions._
import scala.language.experimental.macros
import chisel3.internal._
import chisel3.internal.Builder._
import chisel3.internal.firrtl._
import chisel3.simplechisel._
import chisel3.internal.sourceinfo.{DeprecatedSourceInfo, SourceInfo, SourceInfoTransform, UnlocatableSourceInfo}
import chisel3.internal.sourceinfo.InstTransform
import chisel3.experimental.BaseModule
import _root_.firrtl.annotations.{ModuleName, ModuleTarget, IsModule}
import scala.collection.mutable.HashMap
import chisel3.simplechisel._
import chisel3.internal.firrtl.PrimOp._
import scala.util.control.Breaks._

object Module extends SourceInfoDoc {
  /** A wrapper method that all Module instantiations must be wrapped in
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
    Console.println("Entered module \n\n")
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

    if (Builder.whenDepth != 0) {
      throwException("Internal Error! when() scope depth is != 0, this should have been caught!")
    }
    if (Builder.readyForModuleConstr) {
      throwException("Error: attempted to instantiate a Module, but nothing happened. " +
                     "This is probably due to rewrapping a Module instance with Module()." +
                     sourceInfo.makeMessage(" See " + _))
    }

    module match {
      case m:SimpleChiselModuleInternal =>{
         val md = module.asInstanceOf[SimpleChiselModuleInternal]
         md.generateSimpleChiselComponent
        }
      case _ =>()
    }

    Console.println(s"cmap:${module.simpleChiselConnectionMap}")
    module match{
      case bm:SimpleChiselModuleTrait =>{
        generateSimpleChiselConnection(module)
      }
      case _ =>()
    }

    Builder.currentModule = parent // Back to parent!
    Builder.whenDepth = whenDepth
    Builder.currentClock = saveClock   // Back to clock and reset scope
    Builder.currentReset = saveReset

    var component = module.generateComponent()

    parent match{
      case Some(m) => {
        module match{
          case s:SimpleChiselModuleTrait =>{
            val scm = module.asInstanceOf[SimpleChiselModuleTrait]
            m.simpleChiselConnectionMap += ((scm, new ArrayBuffer[Int]()))
            val bm = module.asInstanceOf[BaseModule]
            bm.simpleChiselSubModuleTrackingId = m.simpleChiselConnectionMap.size - 1
          }
          case _ =>()
        }
      }
      case None => ()
    }
  
    simpleChiselCtrlCheck(component)
    Builder.components += component

    // Handle connections at enclosing scope
    if(!Builder.currentModule.isEmpty) {
      pushCommand(DefInstance(sourceInfo, module, component.ports))
      module.initializeInParent(compileOptions)
    }
    Console.println(s"left module \n\n")
    module
  }

  /** Returns the implicit Clock */
  def clock: Clock = Builder.forcedClock
  /** Returns the implicit Reset */
  def reset: Reset = Builder.forcedReset
  /** Returns the current Module */
  def currentModule: Option[BaseModule] = Builder.currentModule

  def generateSimpleChiselConnection(m: BaseModule)(implicit sourceInfo: SourceInfo,
                                         compileOptions: CompileOptions): Any = {
    m match{
      case _: RawModule => ()
      case _ => {return ()}
    }
    val bm = m.asInstanceOf[RawModule]
    val m_as_scm = m.asInstanceOf[SimpleChiselModuleTrait]
    // easiest thing to do: connect clear bit
    m_as_scm.ctrl match {
      case ctrl_io: TightlyCoupledIOCtrlInternal =>{
        val scm_ctrl_io = m_as_scm.ctrl.asInstanceOf[TightlyCoupledIOCtrlInternal]
        for((m,i)<- bm.simpleChiselConnectionMap){
          m.ctrl match{
            case sub_ctrl_io: TightlyCoupledIOCtrlInternal =>{
              val sub_scm_ctrl_io = m.ctrl.asInstanceOf[TightlyCoupledIOCtrlInternal]
              var found_previous_connection = false
              breakable{
                for((command,idx) <- bm._commands.zipWithIndex){
                  command match{
                    case c:Connect => {
                      if(c.loc.name == sub_scm_ctrl_io.clear.getRef.name){
                        val orOpResult = pushOp(DefPrim(sourceInfo, Bool(), BitOrOp, c.exp, scm_ctrl_io.clear.ref))
                        MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, orOpResult, bm)
                        found_previous_connection = true
                        bm._commands.remove(idx)
                        break
                      }
                    }
                    case _ =>()
                  }
                }
              }
              if(!found_previous_connection){
                MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, scm_ctrl_io.clear, bm)
              }
            }
            case sub_ctrl_io: ValidIOCtrlInternal =>{
              val sub_scm_ctrl_io = m.ctrl.asInstanceOf[ValidIOCtrlInternal]
              var found_previous_connection = false
              breakable{
                for((command,idx) <- bm._commands.zipWithIndex){
                  command match{
                    case c:Connect => {
                      if(c.loc.name == sub_scm_ctrl_io.clear.getRef.name){
                        val orOpResult = pushOp(DefPrim(sourceInfo, Bool(), BitOrOp, c.exp, scm_ctrl_io.clear.ref))
                        MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, orOpResult, bm)
                        found_previous_connection = true
                        bm._commands.remove(idx)
                        break
                      }
                    }
                    case _ =>()
                  }
                }
              }
              if(!found_previous_connection){
                MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, scm_ctrl_io.clear, bm)
              }
            }
            case sub_ctrl_io: DecoupledIOCtrlInternal =>{
              val sub_scm_ctrl_io = m.ctrl.asInstanceOf[DecoupledIOCtrlInternal]
              var found_previous_connection = false
              breakable{
                for((command,idx) <- bm._commands.zipWithIndex){
                  command match{
                    case c:Connect => {
                      if(c.loc.name == sub_scm_ctrl_io.clear.getRef.name){
                        val orOpResult = pushOp(DefPrim(sourceInfo, Bool(), BitOrOp, c.exp, scm_ctrl_io.clear.ref))
                        MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, orOpResult, bm)
                        found_previous_connection = true
                        bm._commands.remove(idx)
                        break
                      }
                    }
                    case _ =>()
                  }
                }
              }
              if(!found_previous_connection){
                MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, scm_ctrl_io.clear, bm)
              }
            }
            case sub_ctrl_io: OutOfOrderIOCtrlInternal =>{
              val sub_scm_ctrl_io = m.ctrl.asInstanceOf[OutOfOrderIOCtrlInternal]
              var found_previous_connection = false
              breakable{
                for((command,idx) <- bm._commands.zipWithIndex){
                  command match{
                    case c:Connect => {
                      if(c.loc.name == sub_scm_ctrl_io.clear.getRef.name){
                        val orOpResult = pushOp(DefPrim(sourceInfo, Bool(), BitOrOp, c.exp, scm_ctrl_io.clear.ref))
                        MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, orOpResult, bm)
                        found_previous_connection = true
                        bm._commands.remove(idx)
                        break
                      }
                    }
                    case _ =>()
                  }
                }
              }
              if(!found_previous_connection){
                MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, scm_ctrl_io.clear, bm)
              }
            }
            case _ =>()
          }
        }
      }
      case ctrl_io: ValidIOCtrlInternal =>{
        val scm_ctrl_io = m_as_scm.ctrl.asInstanceOf[ValidIOCtrlInternal]
        for((m,i)<- bm.simpleChiselConnectionMap){
        m.ctrl match{
            case sub_ctrl_io: TightlyCoupledIOCtrlInternal =>{
              val sub_scm_ctrl_io = m.ctrl.asInstanceOf[TightlyCoupledIOCtrlInternal]
              var found_previous_connection = false
              breakable{
                for((command,idx) <- bm._commands.zipWithIndex){
                  command match{
                    case c:Connect => {
                      if(c.loc.name == sub_scm_ctrl_io.clear.getRef.name){
                        val orOpResult = pushOp(DefPrim(sourceInfo, Bool(), BitOrOp, c.exp, scm_ctrl_io.clear.ref))
                        MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, orOpResult, bm)
                        found_previous_connection = true
                        bm._commands.remove(idx)
                        break
                      }
                    }
                    case _ =>()
                  }
                }
              }
              if(!found_previous_connection){
                MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, scm_ctrl_io.clear, bm)
              }
            }
            case sub_ctrl_io: ValidIOCtrlInternal =>{
              val sub_scm_ctrl_io = m.ctrl.asInstanceOf[ValidIOCtrlInternal]
              var found_previous_connection = false
              breakable{
                for((command,idx) <- bm._commands.zipWithIndex){
                  command match{
                    case c:Connect => {
                      if(c.loc.name == sub_scm_ctrl_io.clear.getRef.name){
                        val orOpResult = pushOp(DefPrim(sourceInfo, Bool(), BitOrOp, c.exp, scm_ctrl_io.clear.ref))
                        MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, orOpResult, bm)
                        found_previous_connection = true
                        bm._commands.remove(idx)
                        break
                      }
                    }
                    case _ =>()
                  }
                }
              }
              if(!found_previous_connection){
                MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, scm_ctrl_io.clear, bm)
              }
            }
            case sub_ctrl_io: DecoupledIOCtrlInternal =>{
              val sub_scm_ctrl_io = m.ctrl.asInstanceOf[DecoupledIOCtrlInternal]
              var found_previous_connection = false
              breakable{
                for((command,idx) <- bm._commands.zipWithIndex){
                  command match{
                    case c:Connect => {
                      if(c.loc.name == sub_scm_ctrl_io.clear.getRef.name){
                        val orOpResult = pushOp(DefPrim(sourceInfo, Bool(), BitOrOp, c.exp, scm_ctrl_io.clear.ref))
                        MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, orOpResult, bm)
                        found_previous_connection = true
                        bm._commands.remove(idx)
                        break
                      }
                    }
                    case _ =>()
                  }
                }
              }
              if(!found_previous_connection){
                MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, scm_ctrl_io.clear, bm)
              }
            }
            case sub_ctrl_io: OutOfOrderIOCtrlInternal =>{
              val sub_scm_ctrl_io = m.ctrl.asInstanceOf[OutOfOrderIOCtrlInternal]
              var found_previous_connection = false
              breakable{
                for((command,idx) <- bm._commands.zipWithIndex){
                  command match{
                    case c:Connect => {
                      if(c.loc.name == sub_scm_ctrl_io.clear.getRef.name){
                        val orOpResult = pushOp(DefPrim(sourceInfo, Bool(), BitOrOp, c.exp, scm_ctrl_io.clear.ref))
                        MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, orOpResult, bm)
                        found_previous_connection = true
                        bm._commands.remove(idx)
                        break
                      }
                    }
                    case _ =>()
                  }
                }
              }
              if(!found_previous_connection){
                MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, scm_ctrl_io.clear, bm)
              }
            }
            case _ =>()
          }
        }
      }
      case ctrl_io: DecoupledIOCtrlInternal =>{
        val scm_ctrl_io = m_as_scm.ctrl.asInstanceOf[DecoupledIOCtrlInternal]
        for((m,i)<- bm.simpleChiselConnectionMap){
        m.ctrl match{
            case sub_ctrl_io: TightlyCoupledIOCtrlInternal =>{
              val sub_scm_ctrl_io = m.ctrl.asInstanceOf[TightlyCoupledIOCtrlInternal]
              var found_previous_connection = false
              breakable{
                for((command,idx) <- bm._commands.zipWithIndex){
                  command match{
                    case c:Connect => {
                      if(c.loc.name == sub_scm_ctrl_io.clear.getRef.name){
                        val orOpResult = pushOp(DefPrim(sourceInfo, Bool(), BitOrOp, c.exp, scm_ctrl_io.clear.ref))
                        MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, orOpResult, bm)
                        found_previous_connection = true
                        bm._commands.remove(idx)
                        break
                      }
                    }
                    case _ =>()
                  }
                }
              }
              if(!found_previous_connection){
                MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, scm_ctrl_io.clear, bm)
              }
            }
            case sub_ctrl_io: ValidIOCtrlInternal =>{
              val sub_scm_ctrl_io = m.ctrl.asInstanceOf[ValidIOCtrlInternal]
              var found_previous_connection = false
              breakable{
                for((command,idx) <- bm._commands.zipWithIndex){
                  command match{
                    case c:Connect => {
                      if(c.loc.name == sub_scm_ctrl_io.clear.getRef.name){
                        val orOpResult = pushOp(DefPrim(sourceInfo, Bool(), BitOrOp, c.exp, scm_ctrl_io.clear.ref))
                        MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, orOpResult, bm)
                        found_previous_connection = true
                        bm._commands.remove(idx)
                        break
                      }
                    }
                    case _ =>()
                  }
                }
              }
              if(!found_previous_connection){
                MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, scm_ctrl_io.clear, bm)
              }
            }
            case sub_ctrl_io: DecoupledIOCtrlInternal =>{
              val sub_scm_ctrl_io = m.ctrl.asInstanceOf[DecoupledIOCtrlInternal]
              var found_previous_connection = false
              breakable{
                for((command,idx) <- bm._commands.zipWithIndex){
                  command match{
                    case c:Connect => {
                      if(c.loc.name == sub_scm_ctrl_io.clear.getRef.name){
                        val orOpResult = pushOp(DefPrim(sourceInfo, Bool(), BitOrOp, c.exp, scm_ctrl_io.clear.ref))
                        MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, orOpResult, bm)
                        found_previous_connection = true
                        bm._commands.remove(idx)
                        break
                      }
                    }
                    case _ =>()
                  }
                }
              }
              if(!found_previous_connection){
                MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, scm_ctrl_io.clear, bm)
              }
            }
            case sub_ctrl_io: OutOfOrderIOCtrlInternal =>{
              val sub_scm_ctrl_io = m.ctrl.asInstanceOf[OutOfOrderIOCtrlInternal]
              var found_previous_connection = false
              breakable{
                for((command,idx) <- bm._commands.zipWithIndex){
                  command match{
                    case c:Connect => {
                      if(c.loc.name == sub_scm_ctrl_io.clear.getRef.name){
                        val orOpResult = pushOp(DefPrim(sourceInfo, Bool(), BitOrOp, c.exp, scm_ctrl_io.clear.ref))
                        MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, orOpResult, bm)
                        found_previous_connection = true
                        bm._commands.remove(idx)
                        break
                      }
                    }
                    case _ =>()
                  }
                }
              }
              if(!found_previous_connection){
                MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, scm_ctrl_io.clear, bm)
              }
            }
            case _ =>()
          }
        }
      }
      case ctrl_io: OutOfOrderIOCtrlInternal =>{
        val scm_ctrl_io = m_as_scm.ctrl.asInstanceOf[OutOfOrderIOCtrlInternal]
        for((m,i)<- bm.simpleChiselConnectionMap){
        m.ctrl match{
            case sub_ctrl_io: TightlyCoupledIOCtrlInternal =>{
              val sub_scm_ctrl_io = m.ctrl.asInstanceOf[TightlyCoupledIOCtrlInternal]
              var found_previous_connection = false
              breakable{
                for((command,idx) <- bm._commands.zipWithIndex){
                  command match{
                    case c:Connect => {
                      if(c.loc.name == sub_scm_ctrl_io.clear.getRef.name){
                        val orOpResult = pushOp(DefPrim(sourceInfo, Bool(), BitOrOp, c.exp, scm_ctrl_io.clear.ref))
                        MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, orOpResult, bm)
                        found_previous_connection = true
                        bm._commands.remove(idx)
                        break
                      }
                    }
                    case _ =>()
                  }
                }
              }
              if(!found_previous_connection){
                MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, scm_ctrl_io.clear, bm)
              }
            }
            case sub_ctrl_io: ValidIOCtrlInternal =>{
              val sub_scm_ctrl_io = m.ctrl.asInstanceOf[ValidIOCtrlInternal]
              var found_previous_connection = false
              breakable{
                for((command,idx) <- bm._commands.zipWithIndex){
                  command match{
                    case c:Connect => {
                      if(c.loc.name == sub_scm_ctrl_io.clear.getRef.name){
                        val orOpResult = pushOp(DefPrim(sourceInfo, Bool(), BitOrOp, c.exp, scm_ctrl_io.clear.ref))
                        MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, orOpResult, bm)
                        found_previous_connection = true
                        bm._commands.remove(idx)
                        break
                      }
                    }
                    case _ =>()
                  }
                }
              }
              if(!found_previous_connection){
                MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, scm_ctrl_io.clear, bm)
              }
            }
            case sub_ctrl_io: DecoupledIOCtrlInternal =>{
              val sub_scm_ctrl_io = m.ctrl.asInstanceOf[DecoupledIOCtrlInternal]
              var found_previous_connection = false
              breakable{
                for((command,idx) <- bm._commands.zipWithIndex){
                  command match{
                    case c:Connect => {
                      if(c.loc.name == sub_scm_ctrl_io.clear.getRef.name){
                        val orOpResult = pushOp(DefPrim(sourceInfo, Bool(), BitOrOp, c.exp, scm_ctrl_io.clear.ref))
                        MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, orOpResult, bm)
                        found_previous_connection = true
                        bm._commands.remove(idx)
                        break
                      }
                    }
                    case _ =>()
                  }
                }
              }
              if(!found_previous_connection){
                MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, scm_ctrl_io.clear, bm)
              }
            }
            case sub_ctrl_io: OutOfOrderIOCtrlInternal =>{
              val sub_scm_ctrl_io = m.ctrl.asInstanceOf[OutOfOrderIOCtrlInternal]
              var found_previous_connection = false
              breakable{
                for((command,idx) <- bm._commands.zipWithIndex){
                  command match{
                    case c:Connect => {
                      if(c.loc.name == sub_scm_ctrl_io.clear.getRef.name){
                        val orOpResult = pushOp(DefPrim(sourceInfo, Bool(), BitOrOp, c.exp, scm_ctrl_io.clear.ref))
                        MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, orOpResult, bm)
                        found_previous_connection = true
                        bm._commands.remove(idx)
                        break
                      }
                    }
                    case _ =>()
                  }
                }
              }
              if(!found_previous_connection){
                MonoConnect.connect(sourceInfo, compileOptions, sub_scm_ctrl_io.clear, scm_ctrl_io.clear, bm)
              }
            }
            case _ =>()
          }
        }
      }
      case _ =>()
    }

    // put all instantiation to the beginning of the module
    var new_commands = ArrayBuffer[Command]()
    var num_of_instance = 0
    for(cmd <- bm._commands){
      cmd match{
        case c: DefInstance =>{
          new_commands.prepend(cmd)
          num_of_instance += 1
        }
        case _ =>{
          new_commands.append(cmd)
        }
      }
    }
    bm._commands = new_commands

    m_as_scm.ctrl.to_module match{ // TODO: Cross-level
      case Some(n_m) =>{
        m_as_scm.ctrl match{
          case ctrl: TightlyCoupledIOCtrlInternal => ()
          case ctrl: ValidIOCtrlInternal => ()
          case ctrl: DecoupledIOCtrlInternal => ()
          case ctrl: OutOfOrderIOCtrlInternal => ()
          case _ => ()
        }
      }
      case None =>
    }

    var next_idx = -1
    val tightlyCoupledRegion = new ArrayBuffer[SimpleChiselModuleTrait]()
    var hasValidIO = false
    var last_idx = -1
    var connectionMap = bm.simpleChiselConnectionMap.clone
    breakable{ // same level
      while(true){
        last_idx = -1
        next_idx = -1
        breakable{
          for(((scm, path),idx) <- connectionMap.zipWithIndex){
            if(path.size!= 0){
              next_idx = idx
            }
          }
        }
        if(next_idx == -1){
          break
        }
        breakable{
          while(true){
            val scm = connectionMap(next_idx)._1
            scm.ctrl match{
              case ctrl: TightlyCoupledIOCtrlInternal =>{
                tightlyCoupledRegion += scm
                if(last_idx != -1){
                  connectionMap(last_idx)._2.remove(0)
                }
                last_idx = next_idx
                if(!connectionMap(next_idx)._2.isEmpty){
                  next_idx = connectionMap(next_idx)._2(0)
                }else{
                  break
                }
              }
              case ctrl: ValidIOCtrlInternal => {
                hasValidIO = true
                tightlyCoupledRegion += scm
                if(last_idx != -1){
                  connectionMap(last_idx)._2.remove(0)
                }
                last_idx = next_idx
                if(!connectionMap(next_idx)._2.isEmpty){
                  next_idx = connectionMap(next_idx)._2(0)
                }else{
                  break
                }
              }
              case ctrl: DecoupledIOCtrlInternal => {
                if(last_idx == -1){
                  connectionMap(next_idx)._2.remove(0)
                }else{
                  if(!connectionMap(next_idx)._2.isEmpty){
                    connectionMap(next_idx)._2.remove(0)
                  }
                  connectionMap(last_idx)._2.remove(0)
                }
                break
              }
              case ctrl: OutOfOrderIOCtrlInternal => {
                if(last_idx == -1){
                  connectionMap(next_idx)._2.remove(0)
                }else{
                  if(!connectionMap(next_idx)._2.isEmpty){
                    connectionMap(next_idx)._2.remove(0)
                  }
                  connectionMap(last_idx)._2.remove(0)
                }
                break              
              }
              case _ => {
                if(last_idx == -1){
                  connectionMap(next_idx)._2.remove(0)
                }else{
                  if(!connectionMap(next_idx)._2.isEmpty){
                    connectionMap(next_idx)._2.remove(0)
                  }
                  connectionMap(last_idx)._2.remove(0)
                }
                break 
              }
            }
          }
        }
        if(hasValidIO){
          // Tackle stuck
          val arrayOfStuck = new ArrayBuffer[Bool]
          for((t,j)<- tightlyCoupledRegion.zipWithIndex){
            var last = false.B
            for((m, i)<- tightlyCoupledRegion.zipWithIndex){
              if(i>j){
                m.ctrl match{
                  case tcIO: TightlyCoupledIOCtrlInternal =>{
                    last = insertOp(DefPrim(sourceInfo, Bool(), BitOrOp, last.ref, tcIO.stuck.ref), num_of_instance)
                    num_of_instance += 1
                  }
                  case validIO: ValidIOCtrlInternal =>{
                    last = insertOp(DefPrim(sourceInfo, Bool(), BitOrOp, last.ref, validIO.stuck.ref), num_of_instance)   
                    num_of_instance += 1
                  }
                  case _ =>()
                }
              }
            }
            arrayOfStuck += last
          }
          var last = false.B
          for(b <- arrayOfStuck){
            last = insertOp(DefPrim(sourceInfo, Bool(), BitOrOp, last.ref, b.ref), num_of_instance)
          }
          for((m, i)<- tightlyCoupledRegion.zipWithIndex){
            val ctrl = m.ctrl.asInstanceOf[TightlyCoupledIOCtrlInternal]
            for((cmd,j) <- bm._commands.zipWithIndex){
              cmd match{
                case c:DefPrim[_] =>{
                  val command = cmd.asInstanceOf[DefPrim[_]]
                  val arg_buf = new ArrayBuffer[Arg]
                  for(arg<- command.args){
                    if(arg.name == ctrl.stuck.ref.name){
                      arg_buf += last.ref
                    }else{
                      arg_buf += arg
                    }
                  } //TODO: fix this
                  // bm._commands.update(j, DefPrim(command.sourceInfo,command.id, command.op, arg_buf.toSeq:_*))
                }
                case c:Connect =>{
                  val command = cmd.asInstanceOf[Connect]
                    if(command.exp.name == ctrl.stuck.ref.name){
                      command.exp = last.ref
                    }
                }
                case _ => ()
              }
            }
          }
          // Tackle stall
          val arrayOfStall = new ArrayBuffer[Bool]
          for((t,j)<- tightlyCoupledRegion.zipWithIndex){
            var last = false.B
            for((m, i)<- tightlyCoupledRegion.zipWithIndex){
              if(i>j){
                m.ctrl match{
                  case tcIO: TightlyCoupledIOCtrlInternal =>{
                    for((cmd,j) <- bm._commands.zipWithIndex){
                      cmd match{
                        case c:Connect =>{
                          val command = cmd.asInstanceOf[Connect]
                          if(command.loc.name == tcIO.stall.ref.name){
                            last = insertOp(DefPrim(sourceInfo, Bool(), BitOrOp, last.ref, command.exp), num_of_instance)   
                            num_of_instance += 1
                          }
                        }
                        case _ => ()
                      }
                    }                    
                  }
                  case validIO: ValidIOCtrlInternal =>{
                    for((cmd,j) <- bm._commands.zipWithIndex){
                      cmd match{
                        case c:Connect =>{
                          val command = cmd.asInstanceOf[Connect]
                          if(command.loc.name == validIO.stall.ref.name){
                            last = insertOp(DefPrim(sourceInfo, Bool(), BitOrOp, last.ref, command.exp), num_of_instance)   
                            num_of_instance += 1
                          }
                        }
                        case _ => ()
                      }
                    }          
                  }
                  case _ =>()
                }
              }
            }
            arrayOfStall += last
          }
          for((m, i)<- tightlyCoupledRegion.zipWithIndex){
            var found_previous_connection = false
            var command_idx = 0
            val ctrl = m.ctrl.asInstanceOf[TightlyCoupledIOCtrlInternal]
            last = pushOp(DefPrim(sourceInfo,Bool(), BitOrOp, arrayOfStall(i).ref,arrayOfStuck(i).ref))
            for((cmd,j) <- bm._commands.zipWithIndex){
              cmd match{
                case c:DefPrim[_] =>{
                  val command = cmd.asInstanceOf[DefPrim[_]]
                  val arg_buf = new ArrayBuffer[Arg]
                  for(arg<- command.args){
                    if(arg.name == ctrl.stall.ref.name){
                      arg_buf += last.ref
                    }else{
                      arg_buf += arg
                    }
                  } //TODO: fix this
                  // bm._commands.update(j, DefPrim(command.sourceInfo,command.id, command.op, arg_buf.toSeq:_*))
                }
                case c:Connect =>{
                  val command = cmd.asInstanceOf[Connect]
                  if(command.loc.name == ctrl.stall.ref.name){
                    val orOpResult = pushOp(DefPrim(sourceInfo, Bool(), BitOrOp, command.exp, last.ref))
                    MonoConnect.connect(sourceInfo, compileOptions, ctrl.stall, orOpResult, bm)
                    found_previous_connection = true
                    command_idx = j
                  }
                }
                case _ => ()
              }
            }
            if(!found_previous_connection){
              MonoConnect.connect(sourceInfo, compileOptions, ctrl.stall, last, bm)
            }else{
              bm._commands.remove(command_idx)
            }
          }
        }else{
          // Tackle stuck
          val arrayOfStuck = new ArrayBuffer[Bool]
          for((t,j)<- tightlyCoupledRegion.zipWithIndex){
            var last = false.B
            for((m, i)<- tightlyCoupledRegion.zipWithIndex){
              val ctrl = m.ctrl.asInstanceOf[TightlyCoupledIOCtrlInternal]
              if(i!=j){
                last = insertOp(DefPrim(sourceInfo, Bool(), BitOrOp, last.ref, ctrl.clear.ref), num_of_instance)
                num_of_instance += 1
              }
            }
            arrayOfStuck += last
          }
          var last = false.B
          for(b <- arrayOfStuck){
            last = insertOp(DefPrim(sourceInfo, Bool(), BitOrOp, last.ref, b.ref), num_of_instance)
          }
          for((m, i)<- tightlyCoupledRegion.zipWithIndex){
            val ctrl = m.ctrl.asInstanceOf[TightlyCoupledIOCtrlInternal]
            for((cmd,j) <- bm._commands.zipWithIndex){
              cmd match{
                case c:DefPrim[_] =>{
                  val command = cmd.asInstanceOf[DefPrim[_]]
                  val arg_buf = new ArrayBuffer[Arg]
                  for(arg<- command.args){
                    if(arg.name == ctrl.stuck.ref.name){
                      arg_buf += last.ref
                    }else{
                      arg_buf += arg
                    }
                  } //TODO: fix this
                  // bm._commands.update(j, DefPrim(command.sourceInfo,command.id, command.op, arg_buf.toSeq:_*))
                }
                case c:Connect =>{
                  val command = cmd.asInstanceOf[Connect]
                    if(command.exp.name == ctrl.stuck.ref.name){
                      command.exp = last.ref
                    }
                }
                case _ => ()
              }
            }
          }
          // Tackle stall
          val arrayOfStall = new ArrayBuffer[Bool]
          for((t,j)<- tightlyCoupledRegion.zipWithIndex){
            var last = false.B
            for((m, i)<- tightlyCoupledRegion.zipWithIndex){
              val ctrl = m.ctrl.asInstanceOf[TightlyCoupledIOCtrlInternal]
              if(i!=j){
                for((cmd,j) <- bm._commands.zipWithIndex){
                  cmd match{
                    case c:Connect =>{
                      val command = cmd.asInstanceOf[Connect]
                      if(command.loc.name == ctrl.stall.ref.name){
                        last = insertOp(DefPrim(sourceInfo, Bool(), BitOrOp, last.ref, command.exp), num_of_instance)   
                        num_of_instance += 1
                      }
                    }
                    case _ => ()
                  }
                }                        
              }
            }
            arrayOfStall += last
          }
          for((m, i)<- tightlyCoupledRegion.zipWithIndex){
            var found_previous_connection = false
            var command_idx = 0
            val ctrl = m.ctrl.asInstanceOf[TightlyCoupledIOCtrlInternal]
            last = pushOp(DefPrim(sourceInfo,Bool(), BitOrOp, arrayOfStall(i).ref,arrayOfStuck(i).ref))
            for((cmd,j) <- bm._commands.zipWithIndex){
              cmd match{
                case c:DefPrim[_] =>{
                  val command = cmd.asInstanceOf[DefPrim[_]]
                  val arg_buf = new ArrayBuffer[Arg]
                  for(arg<- command.args){
                    if(arg.name == ctrl.stall.ref.name){
                      arg_buf += last.ref
                    }else{
                      arg_buf += arg
                    }
                  } //TODO: fix this
                  // bm._commands.update(j, DefPrim(command.sourceInfo,command.id, command.op, arg_buf.toSeq:_*))
                }
                case c:Connect =>{
                  val command = cmd.asInstanceOf[Connect]
                  if(command.loc.name == ctrl.stall.ref.name){
                    val orOpResult = pushOp(DefPrim(sourceInfo, Bool(), BitOrOp, command.exp, last.ref))
                    MonoConnect.connect(sourceInfo, compileOptions, ctrl.stall, orOpResult, bm)
                    found_previous_connection = true
                    command_idx = j
                  }
                }
                case _ => ()
              }
            }
            if(!found_previous_connection){
              MonoConnect.connect(sourceInfo, compileOptions, ctrl.stall, last, bm)
            }else{
              bm._commands.remove(command_idx)
            }
          }
        }
      }
    }
  }
  def simpleChiselCtrlCheck(component: Component): Component = {
    val hashMap: HashMap[String, Int] = new HashMap()
    val lists: ListBuffer[ListBuffer[Int]] = new ListBuffer[ListBuffer[Int]]
    component match{
      case d:DefModule =>{
        def traversePort(elt: Data):Any = elt match {
            case data: Vec[_] => {
                for(e <- data.getElements){
                  if(!hashMap.contains(s"${e.getRef.fullName(d)}")){
                    lists += new ListBuffer[Int]()
                    hashMap += (s"${e.getRef.fullName(d)}" -> (lists.size - 1))
                  }
                }
            }
            case data: Record => {
              for(e <- data.elements.toIndexedSeq){
                traversePort(e._2)
              }
            }
            case _ =>{
              if(!hashMap.contains(s"${elt.getRef.fullName(d)}")){
                lists += new ListBuffer[Int]()
                hashMap += (s"${elt.getRef.fullName(d)}" -> (lists.size - 1))
              }
            }
        }
        def addName(name: String): String = {
          if(!hashMap.contains(name)){
            lists += new ListBuffer[Int]()
            hashMap += (name -> (lists.size - 1))
          }
          name
        }


        var visited: Map[Int, Boolean] = Map()
        var parent: Map[Int, Int] = Map()

        def backtracePath(start: Int, end: Int): ListBuffer[Int] = {
          val p = parent(end)
          if (p == -1) return ListBuffer(start)

          return (backtracePath(start, p) ++ ListBuffer(end))
        }

        def findPath( graph: ListBuffer[ListBuffer[Int]], start: Int, end: Int): Option[ListBuffer[Int]] = {
          val queue = Queue[Int]()

          for (i <- 0 to graph.length) {
            visited += (i -> false)
          }
          queue.enqueue(start)
          parent += (start -> -1)

          while (queue.nonEmpty) {
            val node = queue.dequeue()
            if (node == end) return Some(backtracePath(start, end))
            if (!visited(node)) {
              visited += (node -> true)
              for (neighbor <- graph(node)) {
                parent += (neighbor -> node)
                queue.enqueue(neighbor)
              }

            }
          }

          return None
        }
        var pos_of_valid_in = 0
        var pos_of_valid_out = 0
        var pos_of_ready_in = 0
        var pos_of_ready_out = 0
        var continue = false
        for(port <- d.ports){
          port.id match {
            case ctrl: DecoupledIOCtrlInternal =>{
              continue = true
              lists += new ListBuffer[Int]()
              hashMap += (s"${ctrl.in.valid.getRef.fullName(d)}" -> (lists.size - 1))
              pos_of_valid_in = (lists.size - 1)
              lists += new ListBuffer[Int]()
              hashMap += (s"${ctrl.in.ready.getRef.fullName(d)}" -> (lists.size - 1))
              pos_of_ready_in = (lists.size - 1)
              lists += new ListBuffer[Int]()
              hashMap += (s"${ctrl.out.valid.getRef.fullName(d)}" -> (lists.size - 1))
              pos_of_valid_out = (lists.size - 1)
              lists += new ListBuffer[Int]()
              hashMap += (s"${ctrl.out.ready.getRef.fullName(d)}" -> (lists.size - 1))
              pos_of_ready_out = (lists.size - 1)
              lists += new ListBuffer[Int]()
              hashMap += (s"${ctrl.clear.getRef.fullName(d)}" -> (lists.size - 1))
            }
            case ctrl: OutOfOrderIOCtrlInternal =>{
              continue = true
              lists += new ListBuffer[Int]()
              hashMap += (s"${ctrl.in.valid.getRef.fullName(d)}" -> (lists.size - 1))
              pos_of_valid_in = (lists.size - 1)
              lists += new ListBuffer[Int]()
              hashMap += (s"${ctrl.in.ready.getRef.fullName(d)}" -> (lists.size - 1))
              pos_of_ready_in = (lists.size - 1)
              lists += new ListBuffer[Int]()
              hashMap += (s"${ctrl.out.valid.getRef.fullName(d)}" -> (lists.size - 1))
              pos_of_valid_out = (lists.size - 1)
              lists += new ListBuffer[Int]()
              hashMap += (s"${ctrl.out.ready.getRef.fullName(d)}" -> (lists.size - 1))
              pos_of_ready_out = (lists.size - 1)
              lists += new ListBuffer[Int]()
              hashMap += (s"${ctrl.clear.getRef.fullName(d)}" -> (lists.size - 1))
              lists += new ListBuffer[Int]()
              hashMap += (s"${ctrl.in.ticket_num.getRef.fullName(d)}" -> (lists.size - 1))
              lists += new ListBuffer[Int]()
              hashMap += (s"${ctrl.out.ticket_num.getRef.fullName(d)}" -> (lists.size - 1))
            }
            case _ => traversePort(port.id)
          }
        }
        if(!continue){
          return component
        }
        for(command <- d.commands){
          command match{
            case wire:DefWire =>{
              traversePort(wire.id)
            }
            case _ =>()
          }
        }
        for(command <- d.commands){
          command match{
            case prim:DefPrim[_] =>{              
              traversePort(prim.id)
              val idx_of_dest = hashMap(prim.id.getRef.fullName(d))
              for(arg <- prim.args){
                if(hashMap.contains(arg.fullName(d))){
                  lists(hashMap(arg.fullName(d))) += idx_of_dest
                }
              }
            }
            case _ =>()
          }
        }
        for(command <- d.commands){
          command match{
            case connect:Connect =>{
                if(hashMap.contains(connect.exp.fullName(d))){
                  if(hashMap.contains(connect.loc.fullName(d))){
                    val idx_of_dest = hashMap(connect.loc.fullName(d))
                    lists(hashMap(connect.exp.fullName(d))) += idx_of_dest
                  }
                }
            }
            case connect:ConnectInit =>{
                if(hashMap.contains(connect.exp.fullName(d))){
                  if(hashMap.contains(connect.loc.fullName(d))){
                    val idx_of_dest = hashMap(connect.loc.fullName(d))
                    lists(hashMap(connect.exp.fullName(d))) += idx_of_dest
                  }
                }
            }
            case connect: BulkConnect => ()
            case _ =>()
          }
        }
        val driving_conditional_args = new ListBuffer[Int]()
        for(command <- d.commands){
          command match{
            case w: WhenBegin => {driving_conditional_args += hashMap(w.pred.fullName(d))}
            case w: WhenEnd => {if(!w.hasAlt) driving_conditional_args.clear}
            case o: OtherwiseEnd => driving_conditional_args.clear
            case c: ConnectInit => {
              if(hashMap.contains(c.loc.fullName(d))){
                lists(hashMap(c.loc.fullName(d))) ++= driving_conditional_args
              }
            }
            case c: Connect => {
              if(hashMap.contains(c.loc.fullName(d))){
                lists(hashMap(c.loc.fullName(d)))  ++= driving_conditional_args
              }
            }
            case defP: DefPrim[_] => lists(hashMap(defP.id.getRef.fullName(d)))  ++= driving_conditional_args
            case _ =>()
          }
        }

        findPath(lists, pos_of_valid_in,pos_of_ready_in) match{
          case Some(list) =>{
            //TODO: change to throw expection
            Console.println("Violation found")
          }
          case None =>()
        }
        findPath(lists, pos_of_ready_out,pos_of_valid_out) match{
          case Some(list)=>{
            //TODO: change to throw expection
            Console.println("Violation found")
          }
          case None =>()
        }
      }
    }
    component
  }
}

package experimental {

  object IO {
    /** Constructs a port for the current Module
      *
      * This must wrap the datatype used to set the io field of any Module.
      * i.e. All concrete modules must have defined io in this form:
      * [lazy] val io[: io type] = IO(...[: io type])
      *
      * Items in [] are optional.
      *
      * The granted iodef must be a chisel type and not be bound to hardware.
      *
      * Also registers a Data as a port, also performing bindings. Cannot be called once ports are
      * requested (so that all calls to ports will return the same information).
      * Internal API.
      */
    def apply[T<:Data](iodef: T): T = {
      val module = Module.currentModule.get // Impossible to fail
      require(!module.isClosed, "Can't add more ports after module close")
      requireIsChiselType(iodef, "io type")

      // Clone the IO so we preserve immutability of data types
      val iodefClone = try {
        iodef.cloneTypeFull
      } catch {
        // For now this is going to be just a deprecation so we don't suddenly break everyone's code
        case e: AutoClonetypeException =>
          Builder.deprecated(e.getMessage, Some(s"${iodef.getClass}"))
          iodef
      }
      module.bindIoInPlace(iodefClone)
      iodefClone
    }
  }
}

package internal {
  import chisel3.experimental.BaseModule

  object BaseModule {
    private[chisel3] class ClonePorts (elts: Data*)(implicit compileOptions: CompileOptions) extends Record {
      val elements = ListMap(elts.map(d => d.instanceName -> d.cloneTypeFull): _*)
      def apply(field: String) = elements(field)
      override def cloneType = (new ClonePorts(elts: _*)).asInstanceOf[this.type]
    }

    private[chisel3] def cloneIORecord(proto: BaseModule)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): ClonePorts = {
      require(proto.isClosed, "Can't clone a module before module close")
      val clonePorts = new ClonePorts(proto.getModulePorts: _*)
      clonePorts.bind(WireBinding(Builder.forcedUserModule))
      val cloneInstance = new DefInstance(sourceInfo, proto, proto._component.get.ports) {
        override def name = clonePorts.getRef.name
      }
      pushCommand(cloneInstance)
      if (!compileOptions.explicitInvalidate) {
        pushCommand(DefInvalid(sourceInfo, clonePorts.ref))
      }
      if (proto.isInstanceOf[MultiIOModule]) {
        clonePorts("clock") := Module.clock
        clonePorts("reset") := Module.reset
      }
      clonePorts
    }
  }
}

package experimental {

  /** Abstract base class for Modules, an instantiable organizational unit for RTL.
    */
  // TODO: seal this?
  abstract class BaseModule extends HasId {
    // ArrayBuffer for simpleChisel
    val simpleChiselConnectionMap = new ListBuffer[(SimpleChiselModuleTrait,ArrayBuffer[Int])]
    var simpleChiselSubModuleTrackingId = 0
    //
    // Builder Internals - this tracks which Module RTL construction belongs to.
    //
    if (!Builder.readyForModuleConstr) {
      throwException("Error: attempted to instantiate a Module without wrapping it in Module().")
    }
    readyForModuleConstr = false

    Builder.currentModule = Some(this)
    Builder.whenDepth = 0

    //
    // Module Construction Internals
    //
    protected var _closed = false

    /** Internal check if a Module is closed */
    private[chisel3] def isClosed = _closed

    // Fresh Namespace because in Firrtl, Modules namespaces are disjoint with the global namespace
    private[chisel3] val _namespace = Namespace.empty
    private val _ids = ArrayBuffer[HasId]()
    private[chisel3] def addId(d: HasId) {
      if (Builder.aspectModule(this).isDefined) {
        aspectModule(this).get.addId(d)
      } else {
        require(!_closed, "Can't write to module after module close")
        _ids += d
      }
    }

    protected def getIds = {
      require(_closed, "Can't get ids before module close")
      _ids.toSeq
    }

    private val _ports = new ArrayBuffer[Data]()

    // getPorts unfortunately already used for tester compatibility
    protected[chisel3] def getModulePorts = {
      require(_closed, "Can't get ports before module close")
      _ports.toSeq
    }

    protected[chisel3] def getModuleInputPorts = {
      require(_closed, "Can't get ports before module close")
      val input_ports = new ArrayBuffer[Data]()
      for(port <- _ports){
        val direction = port match {
          case v: Vec[_] => v.specifiedDirection match {
            case SpecifiedDirection.Input => SpecifiedDirection.Input
            case SpecifiedDirection.Output => SpecifiedDirection.Output
            case SpecifiedDirection.Flip => SpecifiedDirection.flip(v.sample_element.specifiedDirection)
            case SpecifiedDirection.Unspecified => v.sample_element.specifiedDirection
          }
          case _ => port.specifiedDirection
        }
        direction match{
            case SpecifiedDirection.Input => input_ports += port
            case _ => ()
        }        
      }
      input_ports.toSeq
    }

    // getPorts unfortunately already used for tester compatibility
    protected[chisel3] def getModuleOutputPorts = {
      require(_closed, "Can't get ports before module close")
      val output_ports = new ArrayBuffer[Data]()
      for(port <- _ports){
        val direction = port match {
          case v: Vec[_] => v.specifiedDirection match {
            case SpecifiedDirection.Input => SpecifiedDirection.Input
            case SpecifiedDirection.Output => SpecifiedDirection.Output
            case SpecifiedDirection.Flip => SpecifiedDirection.flip(v.sample_element.specifiedDirection)
            case SpecifiedDirection.Unspecified => v.sample_element.specifiedDirection
          }
          case _ => port.specifiedDirection
        }
        direction match{
            case SpecifiedDirection.Output => output_ports += port
            case _ => ()
        }        
      }
      output_ports.toSeq
    }


    // These methods allow checking some properties of ports before the module is closed,
    // mainly for compatibility purposes.
    protected def portsContains(elem: Data): Boolean = _ports contains elem

    protected def portsSize: Int = _ports.size

    /** Generates the FIRRTL Component (Module or Blackbox) of this Module.
      * Also closes the module so no more construction can happen inside.
      */
    private[chisel3] def generateComponent(): Component

    /** Sets up this module in the parent context
      */
    private[chisel3] def initializeInParent(parentCompileOptions: CompileOptions): Unit

    //
    // Chisel Internals
    //

    /** The desired name of this module (which will be used in generated FIRRTL IR or Verilog).
      *
      * The name of a module approximates the behavior of the Java Reflection [[`getSimpleName` method
      * https://docs.oracle.com/javase/8/docs/api/java/lang/Class.html#getSimpleName--]] with some modifications:
      *
      * - Anonymous modules will get an `"_Anon"` tag
      * - Modules defined in functions will use their class name and not a numeric name
      *
      * @note If you want a custom or parametric name, override this method.
      */
    def desiredName: String = {
      /* The default module name is derived from the Java reflection derived class name. */
      val baseName = this.getClass.getName

      /* A sequence of string filters applied to the name */
      val filters: Seq[String => String] = Seq(
        ((a: String) => raw"\$$+anon".r.replaceAllIn(a, "_Anon")) // Merge the "$$anon" name with previous name
      )

      filters
        .foldLeft(baseName){ case (str, filter) => filter(str) } // 1. Apply filters to baseName
        .split("\\.|\\$")                                        // 2. Split string at '.' or '$'
        .filterNot(_.forall(_.isDigit))                          // 3. Drop purely numeric names
        .last                                                    // 4. Use the last name
    }

    /** Legalized name of this module. */
    final lazy val name = try {
      Builder.globalNamespace.name(desiredName)
    } catch {
      case e: NullPointerException => throwException(
        s"Error: desiredName of ${this.getClass.getName} is null. Did you evaluate 'name' before all values needed by desiredName were available?", e) // scalastyle:ignore line.size.limit
      case t: Throwable => throw t
    }

    /** Returns a FIRRTL ModuleName that references this object
      *
      * @note Should not be called until circuit elaboration is complete
      */
    final def toNamed: ModuleName = toTarget.toNamed

    /** Returns a FIRRTL ModuleTarget that references this object
      *
      * @note Should not be called until circuit elaboration is complete
      */
    final def toTarget: ModuleTarget = ModuleTarget(this.circuitName, this.name)

    /** Returns a FIRRTL ModuleTarget that references this object
      *
      * @note Should not be called until circuit elaboration is complete
      */
    final def toAbsoluteTarget: IsModule = {
      _parent match {
        case Some(parent) => parent.toAbsoluteTarget.instOf(this.instanceName, toTarget.module)
        case None => toTarget
      }
    }

    /**
      * Internal API. Returns a list of this module's generated top-level ports as a map of a String
      * (FIRRTL name) to the IO object. Only valid after the module is closed.
      *
      * Note: for BlackBoxes (but not ExtModules), this returns the contents of the top-level io
      * object, consistent with what is emitted in FIRRTL.
      *
      * TODO: Use SeqMap/VectorMap when those data structures become available.
      */
    private[chisel3] def getChiselPorts: Seq[(String, Data)] = {
      require(_closed, "Can't get ports before module close")
      _component.get.ports.map { port =>
        (port.id.getRef.asInstanceOf[ModuleIO].name, port.id)
      }
    }

    /** Called at the Module.apply(...) level after this Module has finished elaborating.
      * Returns a map of nodes -> names, for named nodes.
      *
      * Helper method.
      */
    protected def nameIds(rootClass: Class[_]): HashMap[HasId, String] = {
      val names = new HashMap[HasId, String]()

      def name(node: HasId, name: String) {
        // First name takes priority, like suggestName
        // TODO: DRYify with suggestName
        if (!names.contains(node)) {
          names.put(node, name)
        }
      }

      /** Scala generates names like chisel3$util$Queue$$ram for private vals
        * This extracts the part after $$ for names like this and leaves names
        * without $$ unchanged
        */
      def cleanName(name: String): String = name.split("""\$\$""").lastOption.getOrElse(name)

      for (m <- getPublicFields(rootClass)) {
        Builder.nameRecursively(cleanName(m.getName), m.invoke(this), name)
      }

      names
    }

    /** Compatibility function. Allows Chisel2 code which had ports without the IO wrapper to
      * compile under Bindings checks. Does nothing in non-compatibility mode.
      *
      * Should NOT be used elsewhere. This API will NOT last.
      *
      * TODO: remove this, perhaps by removing Bindings checks in compatibility mode.
      */
    def _compatAutoWrapPorts() {} // scalastyle:ignore method.name

    /** Chisel2 code didn't require the IO(...) wrapper and would assign a Chisel type directly to
      * io, then do operations on it. This binds a Chisel type in-place (mutably) as an IO.
      */
    protected def _bindIoInPlace(iodef: Data): Unit = { // scalastyle:ignore method.name
      // Compatibility code: Chisel2 did not require explicit direction on nodes
      // (unspecified treated as output, and flip on nothing was input).
      // This sets assigns the explicit directions required by newer semantics on
      // Bundles defined in compatibility mode.
      // This recursively walks the tree, and assigns directions if no explicit
      // direction given by upper-levels (override Input / Output) AND element is
      // directly inside a compatibility Bundle determined by compile options.
      def assignCompatDir(data: Data, insideCompat: Boolean): Unit = {
        data match {
          case data: Element if insideCompat => data._assignCompatibilityExplicitDirection
          case data: Element => // Not inside a compatibility Bundle, nothing to be done
          case data: Aggregate => data.specifiedDirection match {
            // Recurse into children to ensure explicit direction set somewhere
            case SpecifiedDirection.Unspecified | SpecifiedDirection.Flip => data match {
              case record: Record =>
                val compatRecord = !record.compileOptions.dontAssumeDirectionality
                record.getElements.foreach(assignCompatDir(_, compatRecord))
              case vec: Vec[_] =>
                vec.getElements.foreach(assignCompatDir(_, insideCompat))
            }
            case SpecifiedDirection.Input | SpecifiedDirection.Output => // forced assign, nothing to do
          }
        }
      }

      assignCompatDir(iodef, false)

      iodef.bind(PortBinding(this))
      _ports += iodef
    }

    /** Private accessor for _bindIoInPlace */
    private[chisel3] def bindIoInPlace(iodef: Data): Unit = _bindIoInPlace(iodef)

    /**
      * This must wrap the datatype used to set the io field of any Module.
      * i.e. All concrete modules must have defined io in this form:
      * [lazy] val io[: io type] = IO(...[: io type])
      *
      * Items in [] are optional.
      *
      * The granted iodef must be a chisel type and not be bound to hardware.
      *
      * Also registers a Data as a port, also performing bindings. Cannot be called once ports are
      * requested (so that all calls to ports will return the same information).
      * Internal API.
      *
      * TODO(twigg): Specifically walk the Data definition to call out which nodes
      * are problematic.
      */
    protected def IO[T <: Data](iodef: T): T = chisel3.experimental.IO.apply(iodef) // scalastyle:ignore method.name

    //
    // Internal Functions
    //

    /** Keep component for signal names */
    private[chisel3] var _component: Option[Component] = None

    /** Signal name (for simulation). */
    override def instanceName: String =
      if (_parent == None) name else _component match {
        case None => getRef.name
        case Some(c) => getRef fullName c
      }

  }
}
