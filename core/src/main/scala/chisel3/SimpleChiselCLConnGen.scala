package chisel3.simplechisel

import scala.collection.mutable.{ListBuffer, HashMap, StringBuilder, ArrayBuffer, Set}
import chisel3.internal.sourceinfo.{DeprecatedSourceInfo, SourceInfo, SourceInfoTransform, UnlocatableSourceInfo}
import scala.util.control.Breaks._
import chisel3.internal.firrtl.PrimOp._
import chisel3.experimental.BaseModule
import chisel3.internal._
import chisel3.internal.Builder._
import chisel3.internal.firrtl._
import chisel3._

object SimpleChiselCLConnGen{
    def crossLayerInputValidTranformation(parent: SimpleChiselModuleInternal, 
        parent_ctrl: SimpleChiselIOCtrlInternal, child_ctrl: SimpleChiselIOCtrlInternal): Any ={
        var input_valid_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId.equals(child_ctrl.in.valid.ref.uniqueId)){
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
                    Node(child_ctrl.in.valid, Some(child_ctrl.in.valid.ref.uniqueId)), 
                    parent_ctrl.in.valid.ref))
        }
    }

    def crossLayerInputLSToLITranformation(parent: SimpleChiselModuleInternal, 
        parent_ctrl: LockStepIOCtrlInternal, child_ctrl: LatInsensitiveIOCtrlInternal): Any ={
        var stuck_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId.equals(parent_ctrl.stuck.ref.uniqueId)){
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
                    Node(parent_ctrl.stuck, Some(parent_ctrl.stuck.ref.uniqueId)), 
                    readyNegate.ref))
        }
    }

    def crossLayerInputLIToLITranformation(parent: SimpleChiselModuleInternal, 
        parent_ctrl: LatInsensitiveIOCtrlInternal, child_ctrl: LatInsensitiveIOCtrlInternal): Any ={
        var input_ready_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId.equals(parent_ctrl.in.ready.ref.uniqueId)){
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
                    Node(parent_ctrl.in.ready, Some(parent_ctrl.in.ready.ref.uniqueId)), 
                    child_ctrl.in.ready.ref))
        }
    }

    def crossLayerInputLIToLSTranformation(parent: SimpleChiselModuleInternal, 
        parent_ctrl: LatInsensitiveIOCtrlInternal, child_ctrl: LockStepIOCtrlInternal): Any ={
        var input_ready_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId.equals(parent_ctrl.in.ready.ref.uniqueId)){
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
                    Node(parent_ctrl.in.ready, Some(parent_ctrl.in.ready.ref.uniqueId)), 
                    ready_with_stuck_stall.ref))
        }
    }


    def crossLayerOutputValidTranformation(parent: SimpleChiselModuleInternal, 
        parent_ctrl: SimpleChiselIOCtrlInternal, child_ctrl: SimpleChiselIOCtrlInternal): Any ={
        var input_valid_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId.equals(parent_ctrl.out.valid.ref.uniqueId)){
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
                    Node(parent_ctrl.out.valid, Some(parent_ctrl.out.valid.ref.uniqueId)), 
                    child_ctrl.out.valid.ref))
        }
    }


    def crossLayerOutputLSToLSTranformation(parent: SimpleChiselModuleInternal, 
        parent_ctrl:LockStepIOCtrlInternal , child_ctrl:LockStepIOCtrlInternal): Any ={
        var stall_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId.equals(child_ctrl.stall.ref.uniqueId)){
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
                Node(child_ctrl.stall, Some(child_ctrl.stall.ref.uniqueId)), parent_ctrl.stall.ref))
        }
    }

    def crossLayerOutputLSToLITranformation(parent: SimpleChiselModuleInternal, 
        parent_ctrl:LatInsensitiveIOCtrlInternal , child_ctrl:LockStepIOCtrlInternal): Any ={
        var stall_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId.equals(child_ctrl.stall.ref.uniqueId)){
                            val readyNegate = Builder.insertOp(DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitNotOp, parent_ctrl.out.ready.ref), idx)
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
            val readyNegate = Builder.pushOp(DefPrim(UnlocatableSourceInfo, Bool(),
                    PrimOp.BitNotOp, parent_ctrl.out.ready.ref))
            Builder.pushCommand(
                Connect(UnlocatableSourceInfo, 
                    Node(child_ctrl.stall, Some(child_ctrl.stall.ref.uniqueId)), 
                    readyNegate.ref))
        }
    }

    def crossLayerOutputLIToLITranformation(parent: SimpleChiselModuleInternal, 
        parent_ctrl: LatInsensitiveIOCtrlInternal, child_ctrl: LatInsensitiveIOCtrlInternal): Any ={
        var input_ready_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId.equals(child_ctrl.out.ready.ref.uniqueId)){
                            val ready_bus = Builder.insertOp(
                                        DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitAndOp, connect.exp, parent_ctrl.out.ready.ref), idx)
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
                    Node(child_ctrl.out.ready, Some(child_ctrl.out.ready.ref.uniqueId)), 
                    parent_ctrl.out.ready.ref))
        }
    }

    def crossLayerOutputLIToLSTranformation(parent: SimpleChiselModuleInternal, 
        parent_ctrl: LockStepIOCtrlInternal, child_ctrl: LatInsensitiveIOCtrlInternal): Any ={
        var input_ready_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId.equals(child_ctrl.out.ready.ref.uniqueId)){
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
                    Node(child_ctrl.out.ready, Some(child_ctrl.out.ready.ref.uniqueId)), 
                    ready_with_stuck_stall.ref))
        }
    }

    def apply(m: SimpleChiselModuleInternal): Any ={
        val to_modules: Set[SimpleChiselModuleInternal] = Set()
        for(sub_m <- m.in.to_modules){
            to_modules += sub_m
        }
        for(elt <- m.in.getElements)
        {
            for(sub_m <- elt.to_modules){
                if(!to_modules.contains(sub_m))
                    to_modules += sub_m
            }
        }
        for(sub_m <- to_modules){
            crossLayerInputValidTranformation(m, 
                m.ctrl.asInstanceOf[SimpleChiselIOCtrlInternal], 
                sub_m.ctrl.asInstanceOf[SimpleChiselIOCtrlInternal])
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
        for(sub_m <- m.out.from_modules){
            crossLayerOutputValidTranformation(m, 
                m.ctrl.asInstanceOf[SimpleChiselIOCtrlInternal], 
                sub_m.ctrl.asInstanceOf[SimpleChiselIOCtrlInternal])
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