// See LICENSE for license details.

package chisel3.util

import chisel3._ 
import chisel3.experimental._ 
import chisel3.internal._ 
import chisel3.internal.firrtl._
import chisel3.internal.sourceinfo._

abstract class SimpleChiselIOCtrl extends Bundle{
    def >>>[T <: SimpleChiselIOCtrl](that: T)(implicit sourceInfo: SourceInfo): T
}

class ValidInterface extends Bundle{
    val valid = Input(Bool())
}

class DecoupledInterface extends Bundle{
    val valid = Input(Bool())
    val ready = Output(Bool())
}

class OutOfOrderInterface(val size_of_reorder_buffer: Int) extends Bundle{
    val valid = Input(Bool())
    val ready = Output(Bool())
    val ticket_num = Output(UInt(log2Ceil(size_of_reorder_buffer).W))
}
class NoIOCtrl extends SimpleChiselIOCtrl{

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    override def >>>[T <: SimpleChiselIOCtrl](that: T)(implicit sourceInfo: SourceInfo): T = {
        this >>> that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: NoIOCtrl)(implicit sourceInfo: SourceInfo): NoIOCtrl = {
        that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: TightlyCoupledIOCtrl)(implicit sourceInfo: SourceInfo): TightlyCoupledIOCtrl = {
        that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: ValidIOCtrl)(implicit sourceInfo: SourceInfo): ValidIOCtrl = {
        that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: DecoupledIOCtrl)(implicit sourceInfo: SourceInfo): DecoupledIOCtrl = {
        that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: OutOfOrderIOCtrl)(implicit sourceInfo: SourceInfo): OutOfOrderIOCtrl = {
        that
    }

}

class TightlyCoupledIOCtrl extends SimpleChiselIOCtrl{
    val stall = Input(Bool())
    val clear = Input(Bool())
    val stuck = Output(Bool())

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    override def >>>[T <: SimpleChiselIOCtrl](that: T)(implicit sourceInfo: SourceInfo): T = {
        this >>> that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: NoIOCtrl)(implicit sourceInfo: SourceInfo): NoIOCtrl = {
        that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: TightlyCoupledIOCtrl)(implicit sourceInfo: SourceInfo): TightlyCoupledIOCtrl = {
        this.stall := that.stuck
        that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: ValidIOCtrl)(implicit sourceInfo: SourceInfo): ValidIOCtrl = {
        that.in.valid := this.stuck
        that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: DecoupledIOCtrl)(implicit sourceInfo: SourceInfo): DecoupledIOCtrl = {
        this.stall := !that.in.ready
        that.in.valid := this.stuck
        that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: OutOfOrderIOCtrl)(implicit sourceInfo: SourceInfo): OutOfOrderIOCtrl = {
        this.stall := !that.in.ready
        that.in.valid := this.stuck
        that
    }

}

class ValidIOCtrl extends SimpleChiselIOCtrl{
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
    override def >>>[T <: SimpleChiselIOCtrl](that: T)(implicit sourceInfo: SourceInfo): T = {
        this >>> that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: NoIOCtrl)(implicit sourceInfo: SourceInfo): NoIOCtrl = {
        that
    }


    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: TightlyCoupledIOCtrl)(implicit sourceInfo: SourceInfo): TightlyCoupledIOCtrl = {
        this.stall := that.stuck
        that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: ValidIOCtrl)(implicit sourceInfo: SourceInfo): ValidIOCtrl = {
        this.stall := that.stuck
        that.in.valid := this.out.valid
        that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: DecoupledIOCtrl)(implicit sourceInfo: SourceInfo): DecoupledIOCtrl = {
        this.stall := !that.in.ready
        that.in.valid := this.out.valid
        that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: OutOfOrderIOCtrl)(implicit sourceInfo: SourceInfo): OutOfOrderIOCtrl = {
        this.stall := !that.in.ready
        that.in.valid := this.out.valid
        that
    }
}

class DecoupledIOCtrl extends SimpleChiselIOCtrl {
    val in = new DecoupledInterface
    val out = Flipped(new DecoupledInterface)
    val clear = Input(Bool())


    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: NoIOCtrl)(implicit sourceInfo: SourceInfo): NoIOCtrl = {
        that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    override def >>>[T <: SimpleChiselIOCtrl](that: T)(implicit sourceInfo: SourceInfo): T = {
        this >>> that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: TightlyCoupledIOCtrl)(implicit sourceInfo: SourceInfo): TightlyCoupledIOCtrl = {
        this.out.ready := that.stuck
        that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: ValidIOCtrl)(implicit sourceInfo: SourceInfo): ValidIOCtrl = {
        this.out.ready := that.stuck
        that.in.valid := this.out.valid
        that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: DecoupledIOCtrl)(implicit sourceInfo: SourceInfo): DecoupledIOCtrl = {
        this.out.ready := that.in.ready
        that.in.valid := this.out.valid
        that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: OutOfOrderIOCtrl)(implicit sourceInfo: SourceInfo): OutOfOrderIOCtrl = {
        this.out.ready := that.in.ready
        that.in.valid := this.out.valid
        that
    }
}

class OutOfOrderIOCtrl(val size_of_reorder_buffer: Int) extends SimpleChiselIOCtrl{
    val in = new OutOfOrderInterface(size_of_reorder_buffer)
    val out = Flipped(new OutOfOrderInterface(size_of_reorder_buffer))
    val clear = Input(Bool())

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: NoIOCtrl)(implicit sourceInfo: SourceInfo): NoIOCtrl = {
        that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    override def >>>[T <: SimpleChiselIOCtrl](that: T)(implicit sourceInfo: SourceInfo): T = {
        this >>> that
    }
    
    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: TightlyCoupledIOCtrl)(implicit sourceInfo: SourceInfo): TightlyCoupledIOCtrl = {
        this.out.ready := that.stuck
        that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: ValidIOCtrl)(implicit sourceInfo: SourceInfo): ValidIOCtrl = {
        this.out.ready := that.stuck
        that.in.valid := this.out.valid
        that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: DecoupledIOCtrl)(implicit sourceInfo: SourceInfo): DecoupledIOCtrl = {
        this.out.ready := that.in.ready
        that.in.valid := this.out.valid
        that
    }

    /** Connect this to that $coll based on ctrl connection rules
    * https://github.com/SimpleChisel/simple-chisel-release
    *
    * @param that the $coll to connect to
    */
    def >>>(that: OutOfOrderIOCtrl)(implicit sourceInfo: SourceInfo): OutOfOrderIOCtrl = {
        this.out.ready := that.in.ready
        that.in.valid := this.out.valid
        that
    }
}
