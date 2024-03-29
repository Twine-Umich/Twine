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

object TwineConnGen{

    // Generate TwineConnection with Module m
    def generate(m: TwineModuleBase)(implicit sourceInfo: SourceInfo,
                                         compileOptions: CompileOptions): Any = {
        
        if(m.isInstanceOf[TwineModuleInternal]) TwineCLConnGen(m.asInstanceOf[TwineModuleInternal])

        /**
        By traversing the connectionMap, we would separate all connections into Lock-step Region,
        Decoupled Region, and Cross Region. Lock-step Region need to be reconnected first,
        then decoupled region, then Cross Region
        */
        for(sub_mod <- m.sub_modules){
            val sync_mods: Set[TwineModuleInternal] = Set()
            for(to_mod <- sub_mod.to_modules){
                if(to_mod != m) {
                    TwineSLConnGen(m, sub_mod, to_mod)
                }
                for(sync_m <- to_mod.from_modules){
                    if(sync_m != m && sync_m != sub_mod){
                        if(!sync_mods.contains(sync_m )) sync_mods += sync_m
                    }
                }
            }
            for(sync_mod <- sync_mods){
               TwineSyncConnGen(m, sub_mod, sync_mod)
            }
        }
    }

}