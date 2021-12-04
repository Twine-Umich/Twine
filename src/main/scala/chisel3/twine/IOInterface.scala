// See LICENSE for license details.

package chisel3.twine.util

import chisel3._
import chisel3.util._
import chisel3.twine._
import chisel3.experimental._ 
import chisel3.internal._ 
import chisel3.internal.firrtl._
import chisel3.internal.sourceinfo._

abstract class TwineIOCtrl extends Bundle{

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


class NoIOCtrl extends TwineIOCtrl with NoIOCtrlInternal{

}

class TightlyCoupledIOCtrl(val delay: Int) extends  TwineIOCtrl with TightlyCoupledIOCtrlInternal{
    val stall = Input(Bool())
    val stuck = Output(Bool())
    val in = new ValidInterface
    val out = Flipped(new ValidInterface)
}

class ValidIOCtrl extends  TwineIOCtrl with ValidIOCtrlInternal{
    val in = new ValidInterface
    val out = Flipped(new ValidInterface)
    val stall = Input(Bool())
    val stuck = Output(Bool())
}

class DecoupledIOCtrl(val size_of_receiving_buffer: Int, val size_of_sending_buffer: Int) extends TwineIOCtrl with  DecoupledIOCtrlInternal {
    val in = new DecoupledInterface
    val out = Flipped(new DecoupledInterface)
}

class OutOfOrderIOCtrl(val size_of_reorder_buffer: Int) extends TwineIOCtrl with  OutOfOrderIOCtrlInternal{
    val in = new OutOfOrderInterface(size_of_reorder_buffer)
    val out = Flipped(new OutOfOrderInterface(size_of_reorder_buffer))
}
