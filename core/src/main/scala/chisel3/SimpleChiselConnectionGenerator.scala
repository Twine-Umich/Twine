package chisel3.simplechisel

import scala.collection.mutable.{ListBuffer, HashMap, StringBuilder, ArrayBuffer}
import chisel3.internal.sourceinfo.{DeprecatedSourceInfo, SourceInfo, SourceInfoTransform, UnlocatableSourceInfo}
import scala.util.control.Breaks._
import chisel3.internal.firrtl.PrimOp._
import chisel3.experimental.BaseModule
import chisel3.internal._
import chisel3.internal.Builder._
import chisel3.internal.firrtl._
import chisel3._

object SimpleChiselConnectionGenerator{

    def findDecoupledRegion(rm: RawModule):(ArrayBuffer[(SimpleChiselModuleTrait, SimpleChiselModuleTrait)]) = {
        val allDecoupledRegions = new ArrayBuffer[(SimpleChiselModuleTrait, SimpleChiselModuleTrait)]

        for( source_m <- rm.simpleChiselSubModules){
            for( sink_m <- source_m.to_modules){
                (source_m, sink_m) match{
                    case (source_io: DecoupledIOCtrlInternal, sink_io: DecoupledIOCtrlInternal) =>
                        allDecoupledRegions += ((source_m, sink_m))
                    case (source_io: DecoupledIOCtrlInternal, sink_io: OutOfOrderIOCtrlInternal) =>
                        allDecoupledRegions += ((source_m, sink_m))
                    case (source_io: OutOfOrderIOCtrlInternal, sink_io: DecoupledIOCtrlInternal) =>
                        allDecoupledRegions += ((source_m, sink_m))
                    case (source_io: OutOfOrderIOCtrlInternal, sink_io: OutOfOrderIOCtrlInternal) =>
                        allDecoupledRegions += ((source_m, sink_m))
                    case (_, _) =>() 
                }
            }
        }
        return allDecoupledRegions       
    }

    def findCrossRegion(rm: RawModule):(ArrayBuffer[(SimpleChiselModuleTrait, SimpleChiselModuleTrait)]) = {
        val allCrossRegion = new ArrayBuffer[(SimpleChiselModuleTrait, SimpleChiselModuleTrait)]

        for( source_m <- rm.simpleChiselSubModules){
            for( sink_m <- source_m.to_modules){
                (source_m, sink_m) match{
                    case (source_io: TightlyCoupledIOCtrlInternal, sink_io: DecoupledIOCtrlInternal) =>
                        allCrossRegion += ((source_m, sink_m))
                    case (source_io: TightlyCoupledIOCtrlInternal, sink_io: OutOfOrderIOCtrlInternal) =>
                        allCrossRegion += ((source_m, sink_m))
                    case (source_io: ValidIOCtrlInternal, sink_io: DecoupledIOCtrlInternal) =>
                        allCrossRegion += ((source_m, sink_m))
                    case (source_io: ValidIOCtrlInternal, sink_io: OutOfOrderIOCtrlInternal) =>
                        allCrossRegion += ((source_m, sink_m))
                    case (source_io: DecoupledIOCtrlInternal, sink_io: TightlyCoupledIOCtrlInternal) =>
                        allCrossRegion += ((source_m, sink_m))
                    case (source_io: DecoupledIOCtrlInternal, sink_io: ValidIOCtrlInternal) =>
                        allCrossRegion += ((source_m, sink_m))
                    case (source_io: OutOfOrderIOCtrlInternal, sink_io: TightlyCoupledIOCtrlInternal) =>
                        allCrossRegion += ((source_m, sink_m))
                    case (source_io: OutOfOrderIOCtrlInternal, sink_io: ValidIOCtrlInternal) =>
                        allCrossRegion += ((source_m, sink_m))
                    case (_, _) =>() 
                }
            }
        }
        return allCrossRegion       
    }

    // Takes in a RawModule m and finds all the LockStepRegions within it, the second return type indicating
    // the position of the ValidIO if there is any. If there isn't any ValidIO, set it to -1.
    def findLockStepRegion(rm: RawModule):(ArrayBuffer[ArrayBuffer[SimpleChiselModuleTrait]], ArrayBuffer[Int]) = {

        val allLockStepRegions = new ArrayBuffer[ArrayBuffer[SimpleChiselModuleTrait]]
        val allValidIOIdx = new ArrayBuffer[Int]
        val visited = new HashMap[SimpleChiselModuleTrait, Boolean]

        for(m <- rm.simpleChiselSubModules){
            visited += (m -> false)
        }

        def searchForward(crnt_m: SimpleChiselModuleTrait, 
            crnt_list: ArrayBuffer[SimpleChiselModuleTrait], isValidIO: ArrayBuffer[Boolean] ): (ArrayBuffer[SimpleChiselModuleTrait], ArrayBuffer[Boolean]) = {
                for(forward_m <- crnt_m.from_modules){
                    visited(forward_m) = true
                    forward_m.ctrl match{
                        case ctrl:TightlyCoupledIOCtrlInternal =>{ 
                                crnt_list.prepend(forward_m)
                                isValidIO.prepend(false)
                                return searchForward(forward_m, crnt_list, isValidIO) 
                            }
                        case ctrl:ValidIOCtrlInternal =>{ 
                                crnt_list.prepend(forward_m)
                                isValidIO.prepend(true)
                                return searchForward(forward_m, crnt_list, isValidIO) 
                            }
                        case _ => { return (crnt_list, isValidIO) }
                    }
                }
                return (crnt_list, isValidIO)
        }
        def searchBackward(crnt_m: SimpleChiselModuleTrait, 
            crnt_list: ArrayBuffer[SimpleChiselModuleTrait], isValidIO: ArrayBuffer[Boolean] ): (ArrayBuffer[SimpleChiselModuleTrait], ArrayBuffer[Boolean]) = {
                for(backward_m <- crnt_m.from_modules){
                    visited(backward_m) = true
                    backward_m.ctrl match{
                        case ctrl:TightlyCoupledIOCtrlInternal =>{ 
                                crnt_list += backward_m
                                isValidIO += false
                                return searchBackward(backward_m, crnt_list, isValidIO) 
                            }
                        case ctrl:ValidIOCtrlInternal => { 
                                crnt_list += backward_m
                                isValidIO += true
                                return searchBackward(backward_m, crnt_list, isValidIO) 
                            }
                        case _ => { return  (crnt_list, isValidIO) }
                    }
                } 
                return (crnt_list, isValidIO)
        }

        breakable{
            while(true){
                var crnt_m: Option[SimpleChiselModuleTrait] = None
                breakable{
                    for((m, hasVisited) <- visited){
                        if(!hasVisited){
                            crnt_m = Some(m)
                            break
                        }
                    }
                }

                if(crnt_m.isEmpty){
                    break
                }
                var lockStepRegion = new ArrayBuffer[SimpleChiselModuleTrait]
                var isValidIO = new ArrayBuffer[Boolean]
                visited(crnt_m.get) = true
                crnt_m.get.ctrl match{
                    case ctrl: TightlyCoupledIOCtrlInternal => {
                            val (lockStepRegion_t1, isValidIO_t1) = searchForward(crnt_m.get, lockStepRegion, isValidIO)
                            lockStepRegion = lockStepRegion_t1
                            isValidIO = isValidIO_t1
                            val (lockStepRegion_t2, isValidIO_t2) = searchBackward(crnt_m.get, lockStepRegion, isValidIO)
                            lockStepRegion = lockStepRegion_t2
                            isValidIO = isValidIO_t2
                    }
                    case ctrl: ValidIOCtrlInternal => {
                            val (lockStepRegion_t1, isValidIO_t1) = searchForward(crnt_m.get, lockStepRegion, isValidIO)
                            lockStepRegion = lockStepRegion_t1
                            isValidIO = isValidIO_t1
                            val (lockStepRegion_t2, isValidIO_t2) = searchBackward(crnt_m.get, lockStepRegion, isValidIO)
                            lockStepRegion = lockStepRegion_t2
                            isValidIO = isValidIO_t2
                    }
                    case _ => ()
                }
                if(lockStepRegion.size > 1){
                    allLockStepRegions += lockStepRegion
                    allValidIOIdx += isValidIO.indexOf(true)
                }
            }
        }
        return (allLockStepRegions, allValidIOIdx)
    }

    // Generate SimpleChiselConnection with Module m
    def generate(m: BaseModule)(implicit sourceInfo: SourceInfo,
                                         compileOptions: CompileOptions): Any = {
        m match{
        case _: RawModule => ()
        case _ => {  
            throwException("Currently Simple Chisel only work with RawModule")
            return ()
            }
        }
        // rm = m as RawModule
        // sm = m as simpleChiselModule
        val rm = m.asInstanceOf[RawModule]
        val sm = m.asInstanceOf[SimpleChiselModuleTrait]
        // easiest thing to do: connect clear bit
        sm.ctrl match {
            case ctrl: SimpleChiselIOCtrlInternal =>{
                for(m<- rm.simpleChiselSubModules){
                    m.ctrl match{
                        case sub_ctrl: SimpleChiselIOCtrlInternal =>{
                            var found_previous_connection = false
                            breakable{
                                for((command,idx) <- rm._commands.zipWithIndex){
                                    command match{
                                        case c:Connect => {
                                            if(c.loc.uniqueId == sub_ctrl.clear.getRef.uniqueId){
                                                val orOpResult = pushOp(DefPrim(sourceInfo, Bool(), BitOrOp, c.exp, ctrl.clear.ref))
                                                MonoConnect.connect(sourceInfo, compileOptions, sub_ctrl.clear, orOpResult, rm)
                                                found_previous_connection = true
                                                rm._commands.remove(idx)
                                                break
                                            }
                                        }
                                        case _ =>()
                                    }
                                }
                            }
                            if(!found_previous_connection){
                                MonoConnect.connect(sourceInfo, compileOptions, sub_ctrl.clear, ctrl.clear, rm)
                            }
                        }
                        case _ =>()
                    }
                }
            }
            case _ =>()
        }

        // Put all instantiation to the beginning of the module
        // All instances need to be defined first before we generate connections
        var _new_commands = ArrayBuffer[Command]()
        // Count how many submodules are there, since each submodule is an instance
        var num_of_instance = 0
        for(cmd <- rm._commands){
            cmd match{
                case c: DefInstance =>{
                _new_commands.prepend(cmd)
                num_of_instance += 1
                }
                case _ =>{
                _new_commands.append(cmd)
                }
            }
        }
        // Replace the original commands with the new sequence
        rm._commands = _new_commands

        // If the top module connect to a submodule, then the ctrl need to be auto-connected
        sm.in.to_module match{ // TODO: Cross-level
            case Some(n) =>{
                sm.ctrl match{
                    case ctrl: TightlyCoupledIOCtrlInternal => ()
                    case ctrl: ValidIOCtrlInternal => ()
                    case ctrl: DecoupledIOCtrlInternal => ()
                    case ctrl: OutOfOrderIOCtrlInternal => ()
                    case _ => ()
                }
            }
            case None =>
        }

        /**
        By traversing the connectionMap, we would separate all connections into Lock-step Region,
        Decoupled Region, and Cross Region. Lock-step Region need to be reconnected first,
        then decoupled region, then Cross Region
        */

        // Starting tackling lockStepRegion
        val (allLockStepRegions, allValidIOIdx) = findLockStepRegion(rm)

        for( (lockStepRegion, idx) <- allLockStepRegions.zipWithIndex){
            if( allValidIOIdx(idx) == -1){ // There is no validIO here
                for((m, i) <- lockStepRegion.zipWithIndex){
                    val ctrl = m.ctrl.asInstanceOf[TightlyCoupledIOCtrlInternal]

                    var stuckBus: Option[Bool] = None
                    var stallBus: Option[Arg] = None

                    for((neighbor_m, j) <- lockStepRegion.zipWithIndex){
                        if(i != j){
                            val neighbor_ctrl = neighbor_m.ctrl.asInstanceOf[TightlyCoupledIOCtrlInternal]
                            if(stuckBus.isEmpty){
                                stuckBus = Some(neighbor_ctrl.stuck)
                            }else{
                                stuckBus = Some(insertOp(DefPrim(sourceInfo, Bool(), BitOrOp, stuckBus.get.ref, neighbor_ctrl.stuck.ref), num_of_instance))
                                num_of_instance += 1
                            }
                            for( cmd <- rm._commands){
                                cmd match{
                                    case c:Connect =>{
                                        if(c.loc.uniqueId == neighbor_ctrl.stall.ref.uniqueId){
                                            if(stallBus.isEmpty){
                                                stallBus = Some(c.exp)
                                            }else{
                                                stallBus = Some(insertOp(DefPrim(sourceInfo, Bool(), BitOrOp, stallBus.get, c.exp), num_of_instance).ref)
                                                num_of_instance += 1
                                            }
                                        }
                                    }
                                    case _ => ()
                                }
                            }
                        }
                    }

                    stuckBus = Some(insertOp(DefPrim(sourceInfo, Bool(), BitOrOp, stuckBus.get.ref, ctrl.stuck.ref), num_of_instance))
                    num_of_instance += 1
                    stallBus = Some(insertOp(DefPrim(sourceInfo, Bool(), BitOrOp, stuckBus.get.ref, stallBus.get), num_of_instance).ref)
                    num_of_instance += 1

                    for((cmd,j) <- rm._commands.zipWithIndex){ // Update stuck signal
                        cmd match{
                            case c:DefPrim[_] =>{
                                val arg_buf = new ArrayBuffer[Arg]
                                for(arg<- c.args){
                                    if(arg.uniqueId == ctrl.stuck.ref.uniqueId){
                                        arg_buf += stuckBus.get.ref
                                    }else{
                                        arg_buf += arg
                                    }
                                }
                                rm._commands.update(j, DefPrim(c.sourceInfo, c.id, c.op, arg_buf.toSeq:_*))
                            }
                            case c:Connect =>{
                                if(c.exp.uniqueId == ctrl.stuck.ref.uniqueId){
                                    c.exp = stuckBus.get.ref
                                }
                            }
                            case _ => ()
                        }
                    }
                    for( cmd <- rm._commands){
                        cmd match{
                            case c:Connect =>{
                                if(c.loc.uniqueId == ctrl.stall.ref.uniqueId){
                                    stallBus = Some(insertOp(DefPrim(sourceInfo, Bool(), BitOrOp, stallBus.get, c.exp), num_of_instance).ref)
                                    num_of_instance += 1
                                }
                            }
                            case _ => ()
                        }
                    }
                    for( (cmd, j) <- rm._commands.zipWithIndex){
                        cmd match{
                            case c:Connect =>{
                                if(c.loc.uniqueId == ctrl.stall.ref.uniqueId){
                                    rm._commands.update(j, Connect(sourceInfo, ctrl.stall.lref, stallBus.get))
                                }
                            }
                            case _ => ()
                        }
                    }
                }
            }else{
                for((m, i) <- lockStepRegion.zipWithIndex){
                    val ctrl = m.ctrl.asInstanceOf[TightlyCoupledIOCtrlInternal]

                    var stuckBus: Option[Bool] = None
                    var stallBus: Option[Arg] = None

                    for((neighbor_m, j) <- lockStepRegion.zipWithIndex){
                        if(i < j){
                            val neighbor_ctrl = neighbor_m.ctrl.asInstanceOf[TightlyCoupledIOCtrlInternal]
                            if(stuckBus.isEmpty){
                                stuckBus = Some(neighbor_ctrl.stuck)
                            }else{
                                stuckBus = Some(insertOp(DefPrim(sourceInfo, Bool(), BitOrOp, stuckBus.get.ref, neighbor_ctrl.stuck.ref), num_of_instance))
                                num_of_instance += 1
                            }
                            for( cmd <- rm._commands){
                                cmd match{
                                    case c:Connect =>{
                                        if(c.loc.uniqueId == neighbor_ctrl.stall.ref.uniqueId){
                                            if(stallBus.isEmpty){
                                                stallBus = Some(c.exp)
                                            }else{
                                                stallBus = Some(insertOp(DefPrim(sourceInfo, Bool(), BitOrOp, stallBus.get, c.exp), num_of_instance).ref)
                                                num_of_instance += 1
                                            }
                                        }
                                    }
                                    case _ => ()
                                }
                            }
                        }
                    }

                    stuckBus = Some(insertOp(DefPrim(sourceInfo, Bool(), BitOrOp, stuckBus.get.ref, ctrl.stuck.ref), num_of_instance))
                    num_of_instance += 1
                    stallBus = Some(insertOp(DefPrim(sourceInfo, Bool(), BitOrOp, stuckBus.get.ref, stallBus.get), num_of_instance).ref)
                    num_of_instance += 1

                    for((cmd,j) <- rm._commands.zipWithIndex){ // Update stuck signal
                        cmd match{
                            case c:DefPrim[_] =>{
                                val arg_buf = new ArrayBuffer[Arg]
                                for(arg<- c.args){
                                    if(arg.uniqueId == ctrl.stuck.ref.uniqueId){
                                        arg_buf += stuckBus.get.ref
                                    }else{
                                        arg_buf += arg
                                    }
                                }
                                rm._commands.update(j, DefPrim(c.sourceInfo, c.id, c.op, arg_buf.toSeq:_*))
                            }
                            case c:Connect =>{
                                if(c.exp.uniqueId == ctrl.stuck.ref.uniqueId){
                                    c.exp = stuckBus.get.ref
                                }
                            }
                            case _ => ()
                        }
                    }
                    for( cmd <- rm._commands){
                        cmd match{
                            case c:Connect =>{
                                if(c.loc.uniqueId == ctrl.stall.ref.uniqueId){
                                    stallBus = Some(insertOp(DefPrim(sourceInfo, Bool(), BitOrOp, stallBus.get, c.exp), num_of_instance).ref)
                                    num_of_instance += 1
                                }
                            }
                            case _ => ()
                        }
                    }
                    for( (cmd, j) <- rm._commands.zipWithIndex){
                        cmd match{
                            case c:Connect =>{
                                if(c.loc.uniqueId == ctrl.stall.ref.uniqueId){
                                    rm._commands.update(j, Connect(sourceInfo, ctrl.stall.lref, stallBus.get))
                                }
                            }
                            case _ => ()
                        }
                    }
                }
            }
               
        }
    }

}