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

object SimpleChiselSyncConnGen{

    def syncLSTranformation(parent: SimpleChiselModuleInternal, 
        l_ctrl: SimpleChiselIOCtrlInternal, r_ctrl: LockStepIOCtrlInternal): Any ={
        var stall_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId.equals(r_ctrl.stall.ref.uniqueId)){
                            val validNegate = Builder.insertOp(DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitNotOp, l_ctrl.out.valid.ref), idx)
                            val stall_bus = Builder.insertOp(
                                        DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitOrOp, connect.exp, validNegate.ref), idx+1)
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
            val validNegate = Builder.pushOp(
                    DefPrim(UnlocatableSourceInfo, Bool(),
                            PrimOp.BitNotOp, l_ctrl.out.valid.ref))
            Builder.pushCommand(Connect(UnlocatableSourceInfo, 
                Node(r_ctrl.stall, Some(r_ctrl.stall.ref.uniqueId)), validNegate.ref))
        }
    }

    def syncLITranformation(parent: SimpleChiselModuleInternal, 
        l_ctrl: SimpleChiselIOCtrlInternal, r_ctrl: LatInsensitiveIOCtrlInternal): Any ={
        var output_ready_connected = false
        breakable{
            for((cmd, idx) <- parent._commands.zipWithIndex){
                cmd match{
                    case connect:Connect =>{
                        if(connect.loc.uniqueId.equals(r_ctrl.out.ready.ref.uniqueId)){
                            val ready_bus = Builder.insertOp(
                                        DefPrim(UnlocatableSourceInfo, Bool(),
                                    PrimOp.BitAndOp, connect.exp, l_ctrl.out.valid.ref), idx)
                            connect.exp = ready_bus.ref
                            output_ready_connected = true
                            break
                        }
                    }
                    case _ =>()
                }
            }
        }
        if(!output_ready_connected){
            Builder.pushCommand(
                Connect(UnlocatableSourceInfo, 
                    Node(r_ctrl.out.ready, Some(r_ctrl.out.ready.ref.uniqueId)), 
                    l_ctrl.out.valid.ref))
        }
    }

    def apply(m: SimpleChiselModuleInternal, l_m: SimpleChiselModuleInternal, r_m: SimpleChiselModuleInternal): Any ={
        (l_m.ctrl, r_m.ctrl) match{
            case (l: SimpleChiselIOCtrlInternal, r: LockStepIOCtrlInternal) =>
                syncLSTranformation(m, l, r)
            case (l: SimpleChiselIOCtrlInternal, r: LatInsensitiveIOCtrlInternal) =>
                syncLITranformation(m, l, r)
            case _ =>()
        }
    }
}