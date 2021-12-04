package chisel3.twine

import scala.collection.mutable.{ListBuffer, HashMap, StringBuilder, ArrayBuffer, Set}
import chisel3.internal.sourceinfo.{DeprecatedSourceInfo, SourceInfo, SourceInfoTransform, UnlocatableSourceInfo}
import scala.util.control.Breaks._
import chisel3.internal.firrtl.PrimOp._
import chisel3.experimental.BaseModule
import chisel3.internal._
import chisel3.internal.Builder._
import chisel3.internal.firrtl._
import chisel3._

object TwineCLConnGen{
    def crossLayerInputValidTranformation(parent: TwineModuleInternal, 
        parent_ctrl: TwineIOCtrlInternal, child_ctrl: TwineIOCtrlInternal): Any ={
        var input_valid_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId == child_ctrl.in.valid._id){
                            val valid_bus = Builder.insertOp(
                                        DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitAndOp, connect.exp, parent_ctrl.in.valid.ref), idx)
                            connect.exp = valid_bus.ref
                            input_valid_connected = true
                            break
                        }
                    }
                    case _ =>()
                }
            }
        }
        if(!input_valid_connected){
            Builder.pushCommand(
                Connect(UnlocatableSourceInfo, 
                    Node(child_ctrl.in.valid, child_ctrl.in.valid._id), 
                    parent_ctrl.in.valid.ref))
        }
    }

    def crossLayerInputLSToLITranformation(parent: TwineModuleInternal, 
        parent_ctrl: LockStepIOCtrlInternal, child_ctrl: LatInsensitiveIOCtrlInternal): Any ={
        var stuck_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId == parent_ctrl.stuck._id){
                            val readyNegate = Builder.insertOp(DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitNotOp, child_ctrl.in.ready.ref), idx)
                            val stuckBus = Builder.insertOp(
                                    DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitAndOp, connect.exp, readyNegate.ref), idx+1)
                            connect.exp = stuckBus.ref
                            stuck_connected = true
                            break
                        }
                    }
                    case _ =>()
                }
            }
        }
        if(!stuck_connected){
            val readyNegate = Builder.pushOp(DefPrim(UnlocatableSourceInfo, Bool(),
                    PrimOp.BitNotOp, child_ctrl.in.ready.ref))
            Builder.pushCommand(
                Connect(UnlocatableSourceInfo, 
                    Node(parent_ctrl.stuck, parent_ctrl.stuck._id), 
                    readyNegate.ref))
        }
    }

    def crossLayerInputLIToLITranformation(parent: TwineModuleInternal, 
        parent_ctrl: LatInsensitiveIOCtrlInternal, child_ctrl: LatInsensitiveIOCtrlInternal): Any ={
        var input_ready_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId == parent_ctrl.in.ready._id){
                            val ready_bus = Builder.insertOp(
                                        DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitAndOp, connect.exp, child_ctrl.in.ready.ref), idx)
                            connect.exp = ready_bus.ref
                            input_ready_connected = true
                            break
                        }
                    }
                    case _ =>()
                }
            }
        }
        if(!input_ready_connected){
            Builder.pushCommand(
                Connect(UnlocatableSourceInfo, 
                    Node(parent_ctrl.in.ready, parent_ctrl.in.ready._id), 
                    child_ctrl.in.ready.ref))
        }
    }

    def crossLayerInputLIToLSTranformation(parent: TwineModuleInternal, 
        parent_ctrl: LatInsensitiveIOCtrlInternal, child_ctrl: LockStepIOCtrlInternal): Any ={
        var input_ready_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId == parent_ctrl.in.ready._id){
                            val stuckNegate = Builder.insertOp(DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitNotOp, child_ctrl.stuck.ref), idx)
                            val stallNegate = Builder.insertOp(DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitNotOp, child_ctrl.stall.ref), idx+1)
                            val ready_with_stuck = Builder.insertOp(
                                        DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitAndOp, connect.exp, stuckNegate.ref), idx+2)
                            val ready_with_stuck_stall = Builder.insertOp(
                                        DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitAndOp, ready_with_stuck.ref, stallNegate.ref), idx+3)
                            connect.exp = ready_with_stuck_stall.ref
                            input_ready_connected = true
                            break
                        }
                    }
                    case _ =>()
                }
            }
        }
        if(!input_ready_connected){
            val stuckNegate = Builder.pushOp(DefPrim(UnlocatableSourceInfo, Bool(),
                    PrimOp.BitNotOp, child_ctrl.stuck.ref))
            val stallNegate = Builder.pushOp(DefPrim(UnlocatableSourceInfo, Bool(),
                    PrimOp.BitNotOp, child_ctrl.stall.ref))
            val ready_with_stuck_stall = Builder.pushOp(
                        DefPrim(UnlocatableSourceInfo, Bool(),
                    PrimOp.BitAndOp, stuckNegate.ref, stallNegate.ref))
            Builder.pushCommand(
                Connect(UnlocatableSourceInfo, 
                    Node(parent_ctrl.in.ready, parent_ctrl.in.ready._id), 
                    ready_with_stuck_stall.ref))
        }
    }


    def crossLayerOutputValidTranformation(parent: TwineModuleInternal, 
        parent_ctrl: TwineIOCtrlInternal, child_ctrl: TwineIOCtrlInternal): Any ={
        var input_valid_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId == parent_ctrl.out.valid._id){
                            val valid_bus = Builder.insertOp(
                                        DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitAndOp, connect.exp, child_ctrl.out.valid.ref), idx)
                            connect.exp = valid_bus.ref
                            input_valid_connected = true
                            break
                        }
                    }
                    case _ =>()
                }
            }
        }
        if(!input_valid_connected){
            Builder.pushCommand(
                Connect(UnlocatableSourceInfo, 
                    Node(parent_ctrl.out.valid, parent_ctrl.out.valid._id), 
                    child_ctrl.out.valid.ref))
        }
    }


    def crossLayerOutputLSToLSTranformation(parent: TwineModuleInternal, 
        parent_ctrl:LockStepIOCtrlInternal , child_ctrl:LockStepIOCtrlInternal): Any ={
        var stall_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId == child_ctrl.stall._id){
                            val stall_bus = Builder.insertOp(
                                        DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitOrOp, connect.exp, parent_ctrl.stall.ref), idx)
                            connect.exp = stall_bus.ref
                            stall_connected = true
                            break
                        }
                    }
                    case _ =>()
                }
            }
        }
        if(!stall_connected){
            Builder.pushCommand(Connect(UnlocatableSourceInfo, 
                Node(child_ctrl.stall, child_ctrl.stall._id), parent_ctrl.stall.ref))
        }
    }

    def crossLayerOutputLSToLITranformation(parent: TwineModuleInternal, 
        parent_ctrl:LatInsensitiveIOCtrlInternal , child_ctrl:LockStepIOCtrlInternal): Any ={
        var stall_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId == child_ctrl.stall._id){
                            val p_valid_ready = Builder.pushOp(DefPrim(UnlocatableSourceInfo, Bool(),
                                PrimOp.BitAndOp, parent_ctrl.out.valid.ref, parent_ctrl.out.ready.ref))
                            val readyNegate = Builder.insertOp(DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitNotOp, p_valid_ready.ref), idx)
                            val stallBus = Builder.insertOp(
                                    DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitAndOp, connect.exp, readyNegate.ref), idx+1)
                            connect.exp = stallBus.ref
                            stall_connected = true
                            break
                        }
                    }
                    case _ =>()
                }
            }
        }
        if(!stall_connected){
            val p_valid_ready = Builder.pushOp(DefPrim(UnlocatableSourceInfo, Bool(),
                    PrimOp.BitAndOp, parent_ctrl.out.valid.ref, parent_ctrl.out.ready.ref))
            val readyNegate = Builder.pushOp(DefPrim(UnlocatableSourceInfo, Bool(),
                    PrimOp.BitNotOp, p_valid_ready.ref))
            Builder.pushCommand(
                Connect(UnlocatableSourceInfo, 
                    Node(child_ctrl.stall, child_ctrl.stall._id), 
                    readyNegate.ref))
        }
    }

    def crossLayerOutputLIToLITranformation(parent: TwineModuleInternal, 
        parent_ctrl: LatInsensitiveIOCtrlInternal, child_ctrl: LatInsensitiveIOCtrlInternal): Any ={
        var input_ready_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId == child_ctrl.out.ready._id){
                            val p_valid_ready = Builder.insertOp(DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitAndOp, parent_ctrl.out.valid.ref, parent_ctrl.out.ready.ref), idx)
                            val ready_bus= Builder.insertOp(
                                        DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitAndOp, connect.exp, p_valid_ready.ref), idx+1)
                            connect.exp = ready_bus.ref
                            input_ready_connected = true
                            break
                        }
                    }
                    case _ =>()
                }
            }
        }
        if(!input_ready_connected){
            val p_valid_ready = Builder.pushOp(DefPrim(UnlocatableSourceInfo, Bool(),
                    PrimOp.BitAndOp, parent_ctrl.out.valid.ref, parent_ctrl.out.ready.ref))
            Builder.pushCommand(
                Connect(UnlocatableSourceInfo, 
                    Node(child_ctrl.out.ready, child_ctrl.out.ready._id), 
                    p_valid_ready.ref))
        }
    }

    def crossLayerOutputLIToLSTranformation(parent: TwineModuleInternal, 
        parent_ctrl: LockStepIOCtrlInternal, child_ctrl: LatInsensitiveIOCtrlInternal): Any ={
        var input_ready_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId == child_ctrl.out.ready._id){
                            val stuckNegate = Builder.insertOp(DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitNotOp, parent_ctrl.stuck.ref), idx)
                            val stallNegate = Builder.insertOp(DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitNotOp, parent_ctrl.stall.ref), idx+1)
                            val ready_with_stuck = Builder.insertOp(
                                        DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitAndOp, connect.exp, stuckNegate.ref), idx+2)
                            val ready_with_stuck_stall = Builder.insertOp(
                                        DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitAndOp, ready_with_stuck.ref, stallNegate.ref), idx+3)
                            connect.exp = ready_with_stuck_stall.ref
                            input_ready_connected = true
                            break
                        }
                    }
                    case _ =>()
                }
            }
        }
        if(!input_ready_connected){
            val stuckNegate = Builder.pushOp(DefPrim(UnlocatableSourceInfo, Bool(),
                    PrimOp.BitNotOp, parent_ctrl.stuck.ref))
            val stallNegate = Builder.pushOp(DefPrim(UnlocatableSourceInfo, Bool(),
                    PrimOp.BitNotOp, parent_ctrl.stall.ref))
            val ready_with_stuck_stall = Builder.pushOp(
                        DefPrim(UnlocatableSourceInfo, Bool(),
                    PrimOp.BitAndOp, stuckNegate.ref, stallNegate.ref))
            Builder.pushCommand(
                Connect(UnlocatableSourceInfo, 
                    Node(child_ctrl.out.ready, child_ctrl.out.ready._id), 
                    ready_with_stuck_stall.ref))
        }
    }
    def expandFromModules(aggregate: Aggregate, from_modules:Set[TwineModuleInternal]): Any ={
        for(elt <- aggregate.getElements){
            elt match{
                case a: Aggregate => {
                    for(sub_m <- a.from_modules){
                        if(!from_modules.contains(sub_m))
                            from_modules += sub_m
                    }
                    expandFromModules(a, from_modules)
                }
                case _ => {
                    for(sub_m <- elt.from_modules){
                        if(!from_modules.contains(sub_m))
                            from_modules += sub_m
                    }
                }
            }
        }
    }
    def expandToModules(aggregate: Aggregate, to_modules:Set[TwineModuleInternal]): Any ={
        for(elt <- aggregate.getElements){
            elt match{
                case a: Aggregate => {
                    for(sub_m <- a.to_modules){
                        if(!to_modules.contains(sub_m))
                            to_modules += sub_m
                    }
                    expandFromModules(a, to_modules)
                }
                case _ => {
                    for(sub_m <- elt.to_modules){
                        if(!to_modules.contains(sub_m))
                            to_modules += sub_m
                    }
                }
            }
        }
    }
    def apply(m: TwineModuleInternal): Any ={
        val to_modules: Set[TwineModuleInternal] = Set()
        val from_modules: Set[TwineModuleInternal] = Set()
        expandToModules(m.in, to_modules)
        expandFromModules(m.out,from_modules)

        for(sub_m <- to_modules){
            crossLayerInputValidTranformation(m, 
                m.ctrl.asInstanceOf[TwineIOCtrlInternal], 
                sub_m.ctrl.asInstanceOf[TwineIOCtrlInternal])
            (m.ctrl, sub_m.ctrl) match{
                case (parent_ctrl: LockStepIOCtrlInternal, child_ctrl: LatInsensitiveIOCtrlInternal) =>
                    crossLayerInputLSToLITranformation(m, parent_ctrl, child_ctrl)
                case (parent_ctrl: LatInsensitiveIOCtrlInternal, child_ctrl: LockStepIOCtrlInternal) =>
                    crossLayerInputLIToLSTranformation(m, parent_ctrl, child_ctrl)
                case (parent_ctrl: LatInsensitiveIOCtrlInternal, child_ctrl: LatInsensitiveIOCtrlInternal) =>
                    crossLayerInputLIToLITranformation(m, parent_ctrl, child_ctrl)
                case _ =>()
            }
        }
        for(sub_m <- from_modules){
            crossLayerOutputValidTranformation(m, 
                m.ctrl.asInstanceOf[TwineIOCtrlInternal], 
                sub_m.ctrl.asInstanceOf[TwineIOCtrlInternal])
            (m.ctrl, sub_m.ctrl) match{
                case (parent_ctrl: LockStepIOCtrlInternal, child_ctrl: LockStepIOCtrlInternal) =>
                    crossLayerOutputLSToLSTranformation(m, parent_ctrl, child_ctrl)
                case (parent_ctrl: LockStepIOCtrlInternal, child_ctrl: LatInsensitiveIOCtrlInternal) =>
                    crossLayerOutputLIToLSTranformation(m, parent_ctrl, child_ctrl)
                case (parent_ctrl: LatInsensitiveIOCtrlInternal, child_ctrl: LockStepIOCtrlInternal) =>
                    crossLayerOutputLSToLITranformation(m, parent_ctrl, child_ctrl)
                case (parent_ctrl: LatInsensitiveIOCtrlInternal, child_ctrl: LatInsensitiveIOCtrlInternal) =>
                    crossLayerOutputLIToLITranformation(m, parent_ctrl, child_ctrl)
                case _ =>()
            }
        }
    }
}