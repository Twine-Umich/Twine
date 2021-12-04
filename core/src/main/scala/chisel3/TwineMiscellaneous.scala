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

class TwineMiscellaneous{
    def findDecoupledRegion(sm: TwineModuleInternal):(ArrayBuffer[(TwineModuleInternal, TwineModuleInternal)]) = {
        val allDecoupledRegions = new ArrayBuffer[(TwineModuleInternal, TwineModuleInternal)]

        for( source_m <- sm.sub_modules){
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

    def findCrossRegion(sm: TwineModuleInternal):(ArrayBuffer[(TwineModuleInternal, TwineModuleInternal)]) = {
        val allCrossRegion = new ArrayBuffer[(TwineModuleInternal, TwineModuleInternal)]

        for( source_m <- sm.sub_modules){
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
    def findLockStepRegion(sm: TwineModuleInternal):(ArrayBuffer[ArrayBuffer[TwineModuleInternal]], ArrayBuffer[Int]) = {

        val allLockStepRegions = new ArrayBuffer[ArrayBuffer[TwineModuleInternal]]
        val allValidIOIdx = new ArrayBuffer[Int]
        val visited = new HashMap[TwineModuleInternal, Boolean]

        for(m <- sm.sub_modules){
            visited += (m -> false)
        }

        def searchForward(crnt_m: TwineModuleInternal, 
            crnt_list: ArrayBuffer[TwineModuleInternal], isValidIO: ArrayBuffer[Boolean] ): (ArrayBuffer[TwineModuleInternal], ArrayBuffer[Boolean]) = {
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
        def searchBackward(crnt_m: TwineModuleInternal, 
            crnt_list: ArrayBuffer[TwineModuleInternal], isValidIO: ArrayBuffer[Boolean] ): (ArrayBuffer[TwineModuleInternal], ArrayBuffer[Boolean]) = {
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
                var crnt_m: Option[TwineModuleInternal] = None
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
                var lockStepRegion = new ArrayBuffer[TwineModuleInternal]
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
}