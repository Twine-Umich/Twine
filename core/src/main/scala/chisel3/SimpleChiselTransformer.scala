package chisel3.simplechisel.internal

import chisel3._
import chisel3.internal._
import chisel3.internal.firrtl._
import chisel3.experimental._
import scala.collection.mutable.ArrayBuffer

object SimpleChiselTransformer{
    //replace lId with rId in ctx and return the new ctx
    def replace(lId: Arg, rId: Data, ctx: ArrayBuffer[Command]): ArrayBuffer[Command] ={
        for((cmd, i) <- ctx.zipWithIndex){
            cmd match{
                case dPrim:DefPrim[_] =>{
                    val arg_buf = new ArrayBuffer[Arg]
                    for(arg<- dPrim.args){
                        if(arg.uniqueId.equals(lId.uniqueId)){
                            arg_buf += rId.ref
                        }else{
                            arg_buf += arg
                        }
                    }
                    ctx.update(i, DefPrim(dPrim.sourceInfo, dPrim.id, dPrim.op, arg_buf.toSeq:_*))
                }
                case regInit: DefRegInit =>{
                    if(regInit.init.uniqueId.equals(lId.uniqueId)) regInit.init = rId.ref                   
                }
                case whenBegin: WhenBegin =>{
                    if(whenBegin.pred.uniqueId.equals(lId.uniqueId)) whenBegin.pred = rId.ref                   
                }
                case connect: Connect =>{
                    if(connect.exp.uniqueId.equals(lId.uniqueId)) connect.exp = rId.ref    
                    if(connect.loc.uniqueId.equals(lId.uniqueId)) connect.loc = Node(rId)               
                }
                case connect: ConnectInit =>{
                    if(connect.exp.uniqueId.equals(lId.uniqueId)) connect.exp = rId.ref 
                    if(connect.loc.uniqueId.equals(lId.uniqueId)) connect.loc = Node(rId)                    
                }
                case _ => ()
            }
        }
        return ctx
    }

    //replace lId with rId in ctx and return the new ctx
    def replaceWhen(lId: Arg, rId: Arg, ctx: ArrayBuffer[Command])(pred: () => Boolean): Option[ArrayBuffer[Command]] ={
        if(pred()){
            val new_ctx = new ArrayBuffer[Command]
            return Some(new_ctx)
        }
        else{
            return Some(ctx)
        }
    }

    def removeIfSafe(id: Arg, ctx: ArrayBuffer[Command]): Option[ArrayBuffer[Command]] ={
        //TODO
        return None   
    }

    // find the most front place to insert the cmd
    def Insert(cmd: Command, ctx: ArrayBuffer[Command]): Option[ArrayBuffer[Command]] ={
        //TODO
        return None   
    }

    // Update functions (Optional)
}