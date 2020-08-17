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
                case c:DefInstance =>{
                    if(c.id.getRef.uniqueId.equals(id.uniqueId)){
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

    def findUses(id: Arg, ctx: ArrayBuffer[Command]): Option[Seq[Command]] = {
        //TODO: Implement this
        return None
    }

    def findSameCycleDependencies(id: Arg, ctx: ArrayBuffer[Command]): Option[Seq[Arg]] = {
        //TODO: Implement this
        return None
    }

    def findDependencies(id: Arg, ctx: ArrayBuffer[Command]): Option[Seq[Arg]] = {
        //TODO: Implement this
        return None
    }   

    // Whether lId is dependent on rId
    def hasSameCycleDependency(lId: Arg, rId: Arg, ctx: ArrayBuffer[Command]): Boolean  = {
        //TODO: Implement this
        return false
    } 

    // Whether lId is dependent on rId
    def hasDependency(lId: Arg, rId: Arg, ctx: ArrayBuffer[Command]): Boolean = {
        //TODO: Implement this
        return false
    }

    def findIndirectDependencies(id: Arg, ctx: ArrayBuffer[Command]): Option[Seq[Command]] = {
        //TODO: Implement this
        return None
    }

    def findDirectDependencies(id: Arg, ctx: ArrayBuffer[Command]): Option[Seq[Command]] = {
        //TODO: Implement this
        return None
    }

}