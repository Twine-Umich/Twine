// See LICENSE for license details.

package chisel3.simplechisel.util

import chisel3._
import chisel3.util._
import chisel3.simplechisel._
import chisel3.experimental._ 
import chisel3.internal._ 
import chisel3.internal.firrtl._
import chisel3.internal.sourceinfo._

abstract class SimpleChiselIOCtrl extends Bundle{
  def simpleConnect(that: SimpleChiselIOCtrl): SimpleChiselIOCtrl
  override def >>> (that: Aggregate)(implicit sourceInfo: SourceInfo, connectionCompileOptions:CompileOptions): Aggregate = {
      that match{
          case d:SimpleChiselIOCtrl =>{
              this.simpleConnect(d)
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
    val ticket_num = Input(UInt(log2Ceil(size_of_reorder_buffer).W))
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

class TightlyCoupledIOCtrl(val num_of_cycles: Int) extends  SimpleChiselIOCtrl with TightlyCoupledIOCtrlInternal{
    val stall = Input(Bool())
    val clear = Input(Bool())
    val stuck = Output(Bool())
    val valid_input = Input(Bool())
    val valid_output = Output(Bool())
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
                d.valid_input := this.valid_output
            }
            case d:ValidIOCtrl =>{
                d.in.valid := this.valid_output
            }
            case d:DecoupledIOCtrl =>{
                val ctrlIO = that.asInstanceOf[DecoupledIOCtrl]

            }
            case d:OutOfOrderIOCtrl =>{
                val ctrlIO = that.asInstanceOf[OutOfOrderIOCtrl]
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
                d.valid_input := this.out.valid
            }
            case d:ValidIOCtrl =>{
                d.in.valid := this.out.valid
            }
            case d:DecoupledIOCtrl =>{
                d.in.valid := this.out.valid
            }
            case d:OutOfOrderIOCtrl =>{
                d.in.valid := this.out.valid
            }
        }
        that
    }
}

class DecoupledIOCtrl(val size_of_receiving_buffer: Int, val size_of_sending_buffer: Int) extends SimpleChiselIOCtrl with  DecoupledIOCtrlInternal {
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
                d.valid_input := this.out.valid
            }
            case d:ValidIOCtrl =>{
                d.in.valid := this.out.valid
            }
            case d:DecoupledIOCtrl =>{
                this.out.ready := d.in.ready
                d.in.valid := this.out.valid
            }
            case d:OutOfOrderIOCtrl =>{
                this.out.ready := d.in.ready
                d.in.valid := this.out.valid
                d.in.ticket_num := DontCare
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
                d.valid_input := this.out.valid
            }
            case d:ValidIOCtrl =>{
                d.in.valid := this.out.valid
            }
            case d:DecoupledIOCtrl =>{
                this.out.ready := d.in.ready
                d.in.valid := this.out.valid
            }
            case d:OutOfOrderIOCtrl =>{
                this.out.ready := d.in.ready
                d.in.valid := this.out.valid
                d.in.ticket_num := this.out.ticket_num
            }
        }
        that
    }

}
