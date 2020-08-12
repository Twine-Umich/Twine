package chisel3.simplechisel.internal

import chisel3._
import chisel3.internal._
import chisel3.internal.firrtl._
import chisel3.experimental._
import scala.collection.mutable.ArrayBuffer

object SimpleChiselAnalyzer{
    // Find the definition of a node
    def findDef(id: Arg, ctx: ArrayBuffer[Command]): Option[Command] = {
        for(cmd <- ctx){
            cmd match{
                case c:DefPrim[_] =>{
                    if(c.id.ref.uniqueId.equals(id.uniqueId)){
                        return Some(c)
                    }
                }
                case c:DefWire =>{
                    if(c.id.ref.uniqueId.equals(id.uniqueId)){
                        return Some(c)
                    }
                }
                case c:DefReg =>{
                    if(c.id.ref.uniqueId.equals(id.uniqueId)){
                        return Some(c)
                    }
                }
                case c:Port =>{
                    if(c.id.ref.uniqueId.equals(id.uniqueId)){
                        return Some(c)
                    }
                }
                case c:DefInstance =>{
                    if(c.id.ref.uniqueId.equals(id.uniqueId)){
                        return Some(c)
                    }
                }
                case c:DefRegInit =>{
                    if(c.id.ref.uniqueId.equals(id.uniqueId)){
                        return Some(c)
                    }
                }
                case _ => ()
            }
        }
        return None
    }
}