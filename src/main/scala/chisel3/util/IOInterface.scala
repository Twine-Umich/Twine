// See LICENSE for license details.

package chisel3.util

import chisel3._ 
import chisel3.experimental._ 

trait SimpleChiselIOInterface{}

class ValidInterface extends Bundle{
    val valid = Input(Bool())
}

class DecoupledInterface extends Bundle{
    val valid = Input(Bool())
    val ready = Output(Bool())
}

class TightlyCoupledCtrlInterface extends Bundle{
    val stall = Input(Bool())
    val clear = Input(Bool())
    val stuck = Output(Bool())
}

class ValidCtrlInterface extends Bundle{
    val in = new ValidInterface
    val out = Flipped(new ValidInterface)
    val stall = Input(Bool())
    val clear = Input(Bool())
}

class DecoupledCtrlInterface extends Bundle{
    val in = new DecoupledInterface
    val out = Flipped(new DecoupledInterface)
}

class OutOfOrderCtrlInterface extends Bundle{
    val in = new DecoupledInterface
    val out = Flipped(new DecoupledInterface)
}


trait TightlyCoupledModuleIO 
    extends SimpleChiselIOInterface{
        val ctrl = IO(new TightlyCoupledCtrlInterface)
}

trait ValidModuleIO 
    extends SimpleChiselIOInterface{
        val ctrl = IO(new ValidCtrlInterface)
}

trait DecoupledModuleIO 
    extends SimpleChiselIOInterface{
        val ctrl = IO(new DecoupledCtrlInterface)
}

trait OutOfOrderModuleIO 
    extends SimpleChiselIOInterface{
        val ctrl = IO(new OutOfOrderCtrlInterface)
}

