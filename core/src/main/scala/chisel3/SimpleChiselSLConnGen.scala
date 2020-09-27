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

object SimpleChiselSLConnGen{
    def sameLayerDirectValidTranformation(parent: SimpleChiselModuleBase, 
        l_ctrl: SimpleChiselIOCtrlInternal, r_ctrl: SimpleChiselIOCtrlInternal): Any ={
        var input_valid_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId.equals(r_ctrl.in.valid.ref.uniqueId)){
                            val valid_bus = Builder.insertOp(
                                        DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitAndOp, connect.exp, l_ctrl.out.valid.ref), idx)
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
                    Node(r_ctrl.in.valid, Some(r_ctrl.in.valid.ref.uniqueId)), 
                    l_ctrl.out.valid.ref))
        }
    }

    def sameLayerLSToLSTranformation(parent: SimpleChiselModuleBase, 
        l_ctrl: LockStepIOCtrlInternal, r_ctrl: LockStepIOCtrlInternal): Any ={
        var stall_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId.equals(l_ctrl.stall.ref.uniqueId)){
                            val stall_with_stall = Builder.insertOp(
                                        DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitOrOp, connect.exp, r_ctrl.stall.ref), idx)
                            val stall_bus = Builder.insertOp(
                                        DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitOrOp, stall_with_stall.ref, r_ctrl.stuck.ref), idx+1)
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
            val stall_bus = Builder.pushOp(
                        DefPrim(UnlocatableSourceInfo, Bool(),
                    PrimOp.BitOrOp, r_ctrl.stall.ref, r_ctrl.stuck.ref))
            Builder.pushCommand(Connect(UnlocatableSourceInfo, 
                Node(l_ctrl.stall, Some(l_ctrl.stall.ref.uniqueId)), stall_bus.ref))
        }
    }


    def sameLayerLSToLITranformation(parent: SimpleChiselModuleBase, 
        l_ctrl: LockStepIOCtrlInternal, r_ctrl: LatInsensitiveIOCtrlInternal): Any ={
        var stall_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId.equals(l_ctrl.stall.ref.uniqueId)){
                            val readyNegate = Builder.insertOp(DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitNotOp, r_ctrl.in.ready.ref), idx)
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
                    PrimOp.BitNotOp, r_ctrl.in.ready.ref))
            Builder.pushCommand(
                Connect(UnlocatableSourceInfo, 
                    Node(l_ctrl.stall, Some(l_ctrl.stall.ref.uniqueId)), 
                    readyNegate.ref))
        }
    }

    def sameLayerLIToLSTranformation(parent: SimpleChiselModuleBase, 
        l_ctrl: LatInsensitiveIOCtrlInternal, r_ctrl: LockStepIOCtrlInternal): Any ={
        var ready_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId.equals(l_ctrl.out.ready.ref.uniqueId)){
                            val stuckNegate = Builder.insertOp(DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitNotOp, r_ctrl.stuck.ref), idx)
                            val stallNegate = Builder.insertOp(DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitNotOp, r_ctrl.stall.ref), idx+1)
                            val ready_with_stuck = Builder.insertOp(
                                        DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitAndOp, connect.exp, stuckNegate.ref), idx+2)
                            val ready_with_stuck_stall = Builder.insertOp(
                                        DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitAndOp, ready_with_stuck.ref, stallNegate.ref), idx+3)
                            connect.exp = ready_with_stuck_stall.ref
                            ready_connected = true
                            break
                        }
                    }
                    case _ =>()
                }
            }
        }
        if(!ready_connected){
            val stuckNegate = Builder.pushOp(DefPrim(UnlocatableSourceInfo, Bool(),
                    PrimOp.BitNotOp, r_ctrl.stuck.ref))
            val stallNegate = Builder.pushOp(DefPrim(UnlocatableSourceInfo, Bool(),
                    PrimOp.BitNotOp, r_ctrl.stall.ref))
            val ready_with_stuck_stall = Builder.pushOp(
                        DefPrim(UnlocatableSourceInfo, Bool(),
                    PrimOp.BitAndOp, stuckNegate.ref, stallNegate.ref))
            Builder.pushCommand(
                Connect(UnlocatableSourceInfo, 
                    Node(l_ctrl.out.ready, Some(l_ctrl.out.ready.ref.uniqueId)), 
                    ready_with_stuck_stall.ref))
        }
    }

    def sameLayerLIToLITranformation(parent: SimpleChiselModuleBase, 
        l_ctrl: LatInsensitiveIOCtrlInternal, r_ctrl: LatInsensitiveIOCtrlInternal): Any ={
        var input_ready_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId.equals(l_ctrl.out.ready.ref.uniqueId)){
                            val ready_bus = Builder.insertOp(
                                        DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitAndOp, connect.exp, r_ctrl.in.ready.ref), idx)
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
                    Node(l_ctrl.out.ready, Some(l_ctrl.out.ready.ref.uniqueId)), 
                    r_ctrl.in.ready.ref))
        }
    }

    def apply(m: SimpleChiselModuleBase, l_m: SimpleChiselModuleInternal, r_m: SimpleChiselModuleInternal): Any ={
        sameLayerDirectValidTranformation(m,  l_m.ctrl.asInstanceOf[SimpleChiselIOCtrlInternal], r_m.ctrl.asInstanceOf[SimpleChiselIOCtrlInternal])
        (l_m.ctrl, r_m.ctrl) match{
            case (l: LockStepIOCtrlInternal, r: LockStepIOCtrlInternal) =>
                sameLayerLSToLSTranformation(m, l, r)
            case (l: LockStepIOCtrlInternal, r: LatInsensitiveIOCtrlInternal) =>
                sameLayerLSToLITranformation(m, l, r)
            case (l: LatInsensitiveIOCtrlInternal, r: LockStepIOCtrlInternal) =>
                sameLayerLIToLSTranformation(m, l, r)
            case (l: LatInsensitiveIOCtrlInternal, r: LatInsensitiveIOCtrlInternal) =>{
                sameLayerLIToLITranformation(m, l, r)
                (l, r) match{
                    case(lOutOfOrder: OutOfOrderIOCtrlInternal, rOutOfOrder: OutOfOrderIOCtrlInternal) =>{
                        if(l_m.to_modules.size == 1 && r_m.from_modules.size == 1)             
                        Builder.pushCommand(
                            Connect(UnlocatableSourceInfo, 
                                Node(rOutOfOrder.in.ticket_num, Some(rOutOfOrder.in.ticket_num.ref.uniqueId)), 
                                lOutOfOrder.out.ticket_num.ref))
                    }
                    case _ =>()
                }
            }
            case _ =>()
        }
    }
}