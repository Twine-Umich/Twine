// See LICENSE for license details.

package chisel3.simplechisel.util

import chisel3._
import chisel3.util._
import chisel3.simplechisel._
import chisel3.experimental._ 
import chisel3.internal._ 
import chisel3.internal.firrtl._
import chisel3.internal.sourceinfo._

abstract class SimpleChiselIOCtrl extends Bundle with SimpleChiselIOCtrlInternal{
  def simpleConnect(that: SimpleChiselIOCtrl): SimpleChiselIOCtrl
  override def >>> (that: Aggregate)(implicit sourceInfo: SourceInfo, connectionCompileOptions:CompileOptions): Aggregate = {
      that match{
          case d:SimpleChiselIOCtrl =>{
              val ctrlIO = that.asInstanceOf[SimpleChiselIOCtrl]
              this.simpleConnect(ctrlIO)
          }
          case _ => throwException(s"SimpleChiselIOCtrl can only be connected to SimpleChiselIOCtrl")
      }
      that
  }
}

class ValidInterface extends Bundle with ValidInterfaceInternal{
    val valid = Input(Bool())
}

class DecoupledInterface extends Bundle with DecoupledInterfaceInternal{
    val valid = Input(Bool())
    val ready = Output(Bool())
}

class OutOfOrderInterface(val size_of_reorder_buffer: Int) extends Bundle with OutOfOrderInterfaceInternal{
    val valid = Input(Bool())
    val ready = Output(Bool())
    val ticket_num = Output(UInt(log2Ceil(size_of_reorder_buffer).W))
}


class NoIOCtrl extends SimpleChiselIOCtrl with NoIOCtrlInternal{

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    override def simpleConnect(that: SimpleChiselIOCtrl): SimpleChiselIOCtrl = { 
        that
    }

}

class TightlyCoupledIOCtrl extends  SimpleChiselIOCtrl with TightlyCoupledIOCtrlInternal{
    val stall = Input(Bool())
    val clear = Input(Bool())
    val stuck = Output(Bool())

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    override def simpleConnect(that: SimpleChiselIOCtrl): SimpleChiselIOCtrl =  {
        that match{
            case d:NoIOCtrl =>{
                val ctrlIO = that.asInstanceOf[NoIOCtrl]
            }
            case d:TightlyCoupledIOCtrl =>{
                val ctrlIO = that.asInstanceOf[TightlyCoupledIOCtrl]
                this.stall := ctrlIO.stuck
            }
            case d:ValidIOCtrl =>{
                val ctrlIO = that.asInstanceOf[ValidIOCtrl]
                ctrlIO.in.valid := this.stuck
            }
            case d:DecoupledIOCtrl =>{
                val ctrlIO = that.asInstanceOf[DecoupledIOCtrl]
                this.stall := !ctrlIO.in.ready
                ctrlIO.in.valid := this.stuck
            }
            case d:OutOfOrderIOCtrl =>{
                val ctrlIO = that.asInstanceOf[OutOfOrderIOCtrl]
                this.stall := !ctrlIO.in.ready
                ctrlIO.in.valid := this.stuck
            }
        }
        that
    }



}

class ValidIOCtrl extends  SimpleChiselIOCtrl with ValidIOCtrlInternal{
    val in = new ValidInterface
    val out = Flipped(new ValidInterface)
    val stall = Input(Bool())
    val clear = Input(Bool())
    val stuck = Output(Bool())

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    override def simpleConnect(that: SimpleChiselIOCtrl): SimpleChiselIOCtrl =  {
        that match{
            case d:NoIOCtrl =>{
                val ctrlIO = that.asInstanceOf[NoIOCtrl]
            }
            case d:TightlyCoupledIOCtrl =>{
                val ctrlIO = that.asInstanceOf[TightlyCoupledIOCtrl]
                this.stall := ctrlIO.stuck
            }
            case d:ValidIOCtrl =>{
                val ctrlIO = that.asInstanceOf[ValidIOCtrl]
                this.stall := ctrlIO.stuck
                ctrlIO.in.valid := this.out.valid
            }
            case d:DecoupledIOCtrl =>{
                val ctrlIO = that.asInstanceOf[DecoupledIOCtrl]
                this.stall := !ctrlIO.in.ready
                ctrlIO.in.valid := this.out.valid
            }
            case d:OutOfOrderIOCtrl =>{
                val ctrlIO = that.asInstanceOf[OutOfOrderIOCtrl]
                this.stall := !ctrlIO.in.ready
                ctrlIO.in.valid := this.out.valid
            }
        }
        that
    }
}

class DecoupledIOCtrl extends SimpleChiselIOCtrl with  DecoupledIOCtrlInternal {
    val in = new DecoupledInterface
    val out = Flipped(new DecoupledInterface)
    val clear = Input(Bool())


    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    override def simpleConnect(that: SimpleChiselIOCtrl): SimpleChiselIOCtrl =  {
        that match{
            case d:NoIOCtrl =>{
                val ctrlIO = that.asInstanceOf[NoIOCtrl]
            }
            case d:TightlyCoupledIOCtrl =>{
                val ctrlIO = that.asInstanceOf[TightlyCoupledIOCtrl]
                this.out.ready := ctrlIO.stuck
            }
            case d:ValidIOCtrl =>{
                val ctrlIO = that.asInstanceOf[ValidIOCtrl]
                this.out.ready := ctrlIO.stuck
                ctrlIO.in.valid := this.out.valid
            }
            case d:DecoupledIOCtrl =>{
                val ctrlIO = that.asInstanceOf[DecoupledIOCtrl]
                this.out.ready := ctrlIO.in.ready
                ctrlIO.in.valid := this.out.valid
            }
            case d:OutOfOrderIOCtrl =>{
                val ctrlIO = that.asInstanceOf[OutOfOrderIOCtrl]
                this.out.ready := ctrlIO.in.ready
                ctrlIO.in.valid := this.out.valid
            }
        }
        that
    }
}

class OutOfOrderIOCtrl(val size_of_reorder_buffer: Int) extends SimpleChiselIOCtrl with  OutOfOrderIOCtrlInternal{
    val in = new OutOfOrderInterface(size_of_reorder_buffer)
    val out = Flipped(new OutOfOrderInterface(size_of_reorder_buffer))
    val clear = Input(Bool())

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    override def simpleConnect(that: SimpleChiselIOCtrl): SimpleChiselIOCtrl = {
        that match{
            case d:NoIOCtrl =>{
                val ctrlIO = that.asInstanceOf[NoIOCtrl]
            }
            case d:TightlyCoupledIOCtrl =>{
                val ctrlIO = that.asInstanceOf[TightlyCoupledIOCtrl]
                this.out.ready := ctrlIO.stuck
            }
            case d:ValidIOCtrl =>{
                val ctrlIO = that.asInstanceOf[ValidIOCtrl]
                this.out.ready := ctrlIO.stuck
                ctrlIO.in.valid := this.out.valid
            }
            case d:DecoupledIOCtrl =>{
                val ctrlIO = that.asInstanceOf[DecoupledIOCtrl]
                this.out.ready := ctrlIO.in.ready
                ctrlIO.in.valid := this.out.valid
            }
            case d:OutOfOrderIOCtrl =>{
                val ctrlIO = that.asInstanceOf[OutOfOrderIOCtrl]
                this.out.ready := ctrlIO.in.ready
                ctrlIO.in.valid := this.out.valid
            }
        }
        that
    }

}
