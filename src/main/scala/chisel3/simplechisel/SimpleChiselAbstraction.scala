// See LICENSE for license details.

package chisel3.simplechisel.util

import chisel3._ 
import chisel3.simplechisel._
import chisel3.internal._ 
import chisel3.internal.firrtl._
import chisel3.experimental._ 
import chisel3.internal.sourceinfo._
import chisel3.util._
/** Abstract base class for SimpleChiselState, which only contains basic sequential blocks.
  * These may contain both logic and state which are written in the Module
  * body (constructor).
  * This abstract base class includes an implicit clock and reset.
  *
  * @note SimpleChiselState instantiations must be wrapped in a State() call.
  * @note SimpleChiselState must implement one of the four SimpleChiselIOCtrl as ctrl.
  */
abstract class SimpleChiselState(implicit moduleCompileOptions: CompileOptions)
  extends SimpleChiselStateInternal{
    override def ctrl: SimpleChiselIOCtrl
    
    private[chisel3] def generateSimpleChiselComponent(): Any = {
        if(!this.simpleChiselSubModules.isEmpty){
          return
        }
        ctrl match{
          case d:TightlyCoupledIOCtrl =>{
            if(d.num_of_cycles > 0){
              val tightlyCoupledQ = Reg(Vec(d.num_of_cycles, Bool()))
              when( (d.stall||d.stuck) ){
                tightlyCoupledQ(0) := d.valid_input
              }.otherwise{
                tightlyCoupledQ(0) := tightlyCoupledQ(0)
              }
              for(i <- 1 until d.num_of_cycles){
                when( (d.stall||d.stuck)){
                  tightlyCoupledQ(i) := tightlyCoupledQ(i-1)
                }.otherwise{
                  tightlyCoupledQ(i) := tightlyCoupledQ(i)
                }
              }
              d.valid_output := tightlyCoupledQ(d.num_of_cycles - 1)
            }else{
              d.valid_output := d.valid_input
            }
          }
          case d:DecoupledIOCtrl =>{
            val input_buffer = new Queue(chiselTypeOf(this.in), d.size_of_receiving_buffer, true, true)
            val output_buffer = new Queue(chiselTypeOf(this.in), d.size_of_receiving_buffer, true, true)
          }
          case _ => ()
        }
    }

    private[chisel3] override def generateComponent(): Component = {

        // Restrict IO to must have ctrl, in, and out
        require(in != null, "Module must have in")
        require(out != null, "Module must have out")
        require(ctrl != null, "SimpleChisel Module must have ctrl")
        require(portsContains(in), "Module must have in wrapped in IO(...)")
        require(portsContains(out), "Module must have out wrapped in IO(...)")
        require(portsContains(ctrl), "Module must have ctrl wrapped in IO(...)")
        // require(portsSize == 5, "Module must only have in, out, ctrl, clock, and reset as IO")

        super.generateComponent()
    }

    /** Connect this to that $coll mono-directionally hand side and element-wise.
    *
    * @param that the $coll to connect to
    * @group Connect
    */
    def >>>[T <: SimpleChiselModuleTrait](that: T): T ={
        implicit val sourceInfo = UnlocatableSourceInfo
        val input_ports = that.in.getElements
        val output_ports = this.out.getElements
        if(input_ports.size != output_ports.size){
          throwException("The input does not match with outputs")
        }
        for((input_port, idx) <- input_ports.zipWithIndex){
          input_port.connect(output_ports(idx))(sourceInfo, moduleCompileOptions)
        }
        this.to_modules += that
        that.from_modules += this

        this.ctrl >>> that.ctrl

        that
    }

    /** Connect this to that $coll mono-directionally hand side and element-wise.
      *
      * @param that the $coll to connect to
      * @group Connect
      */
    def >>> (that: Aggregate): Aggregate = {
        implicit val sourceInfo = UnlocatableSourceInfo
        val input_ports = that.getElements
        val output_ports = this.out.getElements
        if(input_ports.size != output_ports.size){
          throwException("The input does not match with outputs")
        }
        for((input_port, idx) <- input_ports.zipWithIndex){
          input_port.connect(output_ports(idx))(sourceInfo, moduleCompileOptions)
        }
        that.from_module = Some(this)
        that
    }
}

/** Abstract base class for SimpleChiselLogic, which only contains basic combinational blocks.
  * These may contain logic which are written in the Module
  * body (constructor).
  * This abstract base class includes an implicit clock and reset.
  *
  * 
  * @note SimpleChiselLogic instantiations must be wrapped in a Logic() call.
  * @note SimpleChiselLogic must implement one of the four SimpleChiselIOCtrl as ctrl.
  */
abstract class SimpleChiselLogic(implicit moduleCompileOptions: CompileOptions)
  extends SimpleChiselLogicInternal{
    override def ctrl: SimpleChiselIOCtrl

    private[chisel3] def generateSimpleChiselComponent(): Any = {
        if(!this.simpleChiselSubModules.isEmpty){
          return
        }
        ctrl match{
          case d:TightlyCoupledIOCtrl =>{
            if(d.num_of_cycles > 0){
              val tightlyCoupledQ = Reg(Vec(d.num_of_cycles, Bool()))
              when( (d.stall||d.stuck) ){
                tightlyCoupledQ(0) := d.valid_input
              }.otherwise{
                tightlyCoupledQ(0) := tightlyCoupledQ(0)
              }
              for(i <- 1 until d.num_of_cycles){
                when( (d.stall||d.stuck)){
                  tightlyCoupledQ(i) := tightlyCoupledQ(i-1)
                }.otherwise{
                  tightlyCoupledQ(i) := tightlyCoupledQ(i)
                }
              }
              d.valid_output := tightlyCoupledQ(d.num_of_cycles - 1)
            }else{
              d.valid_output := d.valid_input
            }
          }
          case _ => ()
        }
    }
    
    private[chisel3] override def generateComponent(): Component = {

        // Restrict IO to must have ctrl, in, and out
        require(in != null, "Module must have in")
        require(out != null, "Module must have out")
        require(ctrl != null, "SimpleChisel Module must have ctrl")
        require(portsContains(in), "Module must have in wrapped in IO(...)")
        require(portsContains(out), "Module must have out wrapped in IO(...)")
        require(portsContains(ctrl), "Module must have ctrl wrapped in IO(...)")
        // require(portsSize == 5, "Module must only have in, out, ctrl, clock, and reset as IO")
        
        super.generateComponent()
    }

    /** Connect this to that $coll mono-directionally hand side and element-wise.
    *
    * @param that the $coll to connect to
    * @group Connect
    */
    def >>>[T <: SimpleChiselModuleTrait](that: T): T ={
        implicit val sourceInfo = UnlocatableSourceInfo
        val input_ports = that.in.getElements
        val output_ports = this.out.getElements
        if(input_ports.size != output_ports.size){
          throwException("The input does not match with outputs")
        }
        for((input_port, idx) <- input_ports.zipWithIndex){
          input_port.connect(output_ports(idx))(sourceInfo, moduleCompileOptions)
        }
        this.to_modules += that
        that.from_modules += this
        this.ctrl >>> that.ctrl

        that
    }
  /** Connect this to that $coll mono-directionally hand side and element-wise.
    *
    * @param that the $coll to connect to
    * @group Connect
    */
  def >>> (that: Aggregate): Aggregate = {
      implicit val sourceInfo = UnlocatableSourceInfo
      val input_ports = that.getElements
      val output_ports = this.out.getElements
      if(input_ports.size != output_ports.size){
        throwException("The input does not match with outputs")
      }
      for((input_port, idx) <- input_ports.zipWithIndex){
        input_port.connect(output_ports(idx))(sourceInfo, moduleCompileOptions)
      }
      that.from_module = Some(this)
      that
  }
}

/** Abstract base class for SimpleChiselModule, which behave much like Verilog modules.
  * These may contain both logic and state which are written in the Module
  * body (constructor).
  * This abstract base class includes an implicit clock and reset.
  *
  * @note SimpleChiselModule instantiations must be wrapped in a Module() call.
  * @note SimpleChiselModule must implement one of the four SimpleChiselIOCtrl as ctrl.  
  */
abstract class SimpleChiselModule(implicit moduleCompileOptions: CompileOptions)
  extends SimpleChiselModuleInternal{
    override def ctrl: SimpleChiselIOCtrl

    private[chisel3] def generateSimpleChiselComponent(): Any = {
        if(!this.simpleChiselSubModules.isEmpty){
          return
        }
        ctrl match{
          case d:TightlyCoupledIOCtrl =>{
            if(d.num_of_cycles > 0){
              val tightlyCoupledQ = Reg(Vec(d.num_of_cycles, Bool()))
              when( (d.stall||d.stuck) ){
                tightlyCoupledQ(0) := tightlyCoupledQ(0)
                d.valid_output := false.B
              }.otherwise{
                tightlyCoupledQ(0) := d.valid_input
                d.valid_output := tightlyCoupledQ(d.num_of_cycles - 1)
              }
              for(i <- 1 until d.num_of_cycles){
                when( (d.stall||d.stuck)){
                  tightlyCoupledQ(i) := tightlyCoupledQ(i-1)
                }.otherwise{
                  tightlyCoupledQ(i) := tightlyCoupledQ(i)
                }
              }
            }else{
              when((d.stall||d.stuck)){
                d.valid_output := false.B
              }.otherwise{
                d.valid_output := d.valid_input
              }
            }
          }
          case _ => ()
        }
    }
  
    private[chisel3] override def generateComponent(): Component = {
        // Restrict IO to must have ctrl, in, and out
        require(in != null, "Module must have in")
        require(out != null, "Module must have out")
        require(ctrl != null, "SimpleChisel Module must have ctrl")
        require(portsContains(in), "Module must have in wrapped in IO(...)")
        require(portsContains(out), "Module must have out wrapped in IO(...)")
        require(portsContains(ctrl), "Module must have ctrl wrapped in IO(...)")
        // require(portsSize == 5, "Module must only have in, out, ctrl, clock, and reset as IO")
        
        super.generateComponent()
    }

    /** Connect this to that $coll mono-directionally hand side and element-wise.
    *
    * @param that the $coll to connect to
    * @group Connect
    */
    def >>>[T <: SimpleChiselModuleTrait](that: T): T ={
        implicit val sourceInfo = UnlocatableSourceInfo
        val input_ports = that.in.getElements
        val output_ports = this.out.getElements
        if(input_ports.size != output_ports.size){
          throwException("The input does not match with outputs")
        }
        for((input_port, idx) <- input_ports.zipWithIndex){
          input_port.connect(output_ports(idx))(sourceInfo, moduleCompileOptions)
        }
        this.to_modules += that
        that.from_modules += this
        this.ctrl >>> that.ctrl

        that
    }

  /** Connect this to that $coll mono-directionally hand side and element-wise.
    *
    * @param that the $coll to connect to
    * @group Connect
    */
  def >>> (that: Aggregate): Aggregate = {
      implicit val sourceInfo = UnlocatableSourceInfo
      val input_ports = that.getElements
      val output_ports = this.out.getElements
      if(input_ports.size != output_ports.size){
        throwException("The input does not match with outputs")
      }
      for((input_port, idx) <- input_ports.zipWithIndex){
        input_port.connect(output_ports(idx))(sourceInfo, moduleCompileOptions)
      }
      that.from_module = Some(this)
      that
  }
}