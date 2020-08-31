// See LICENSE for license details.

package chisel3.simplechisel.util

import chisel3._ 
import chisel3.simplechisel._
import chisel3.internal._ 
import chisel3.internal.firrtl._
import chisel3.experimental._ 
import chisel3.internal.sourceinfo._
import chisel3.util._
import chisel3.simplechisel.internal._
import chisel3.internal.MonoConnect.MissingFieldException

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

    /** Connect this to that $coll mono-directionally hand side and element-wise.
    *
    * @param that the $coll to connect to
    * @group Connect
    */
    override def >>>[T <: SimpleChiselModuleTrait](that: T): T ={
        implicit val sourceInfo = UnlocatableSourceInfo
        val input_ports = that.in.getElements
        val output_ports = this.out.getElements

        if(input_ports.size != output_ports.size){
          throwException("The input does not match with outputs")
        }
        for((field, sink_sub) <- that.in.elements) {
          this.out.elements.get(field) match {
            case Some(source_sub) => {
              (sink_sub, source_sub) match{
                case(sink_vec: Vec[Data @unchecked], source_e: Element) =>{
                  val parallelizer = Module(new Parallelizer(source_e, sink_vec.length))
                  this.ctrl >>> parallelizer.ctrl >>> that.ctrl
                  parallelizer.in.bits := source_e
                  sink_vec := parallelizer.out.bits
                }
                case(sink_e: Element, source_vec: Vec[Data @unchecked]) =>{
                  val serializer = Module(new Serializer(sink_e, source_vec.length))
                  this.ctrl >>> serializer.ctrl >>> that.ctrl
                  serializer.in.bits := source_vec
                  sink_e := serializer.out.bits
                } // Add the transformation cases
                case _ =>
                  sink_sub.connect(source_sub)(sourceInfo,  moduleCompileOptions)
              }
            }
            case None => {
                throw MissingFieldException(field)
            }
          }
        }

        this.to_modules += that
        that.from_modules += this
        this.ctrl >>> that.ctrl

        that
    }

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
          case d:DecoupledIOCtrl =>{
            // Only generate the queue if the receiving or sending buffer is not of size 0
            if(d.size_of_receiving_buffer != 0){
              val input_buffer = Module(new Queue(chiselTypeOf(this.in), d.size_of_receiving_buffer, true, true))
              for((str,d) <- this.in.elements){
                SimpleChiselTransformer.replaceAll(d.ref, input_buffer.io.deq.bits.elements(str), this._commands)
              }
              SimpleChiselTransformer.replaceAll(d.in.valid.ref, input_buffer.io.deq.valid, this._commands)
              SimpleChiselTransformer.replaceAll(d.in.ready.ref, input_buffer.io.deq.ready, this._commands)
              input_buffer.io.enq.valid := d.in.valid
              d.in.ready := input_buffer.io.enq.ready
              this.in <> input_buffer.io.enq.bits
            }

            if(d.size_of_sending_buffer != 0){
              val output_buffer = Module(new Queue(chiselTypeOf(this.out), d.size_of_sending_buffer, true, true))

              for((str,d) <- this.out.elements){
                SimpleChiselTransformer.replaceUses(d.ref, output_buffer.io.enq.bits.elements(str), this._commands)
              }
              SimpleChiselTransformer.replaceAll(d.out.valid.ref, output_buffer.io.enq.valid, this._commands)
              SimpleChiselTransformer.replaceAll(d.out.ready.ref, output_buffer.io.enq.ready, this._commands)
              output_buffer.io.deq.ready := d.out.ready
              d.out.valid :=  output_buffer.io.deq.valid
              this.out <> output_buffer.io.deq.bits
            }
          }
          case d:OutOfOrderIOCtrl =>{
            // Only generate the queue if the re-order buffer size is not of size 0
            if(d.size_of_reorder_buffer != 0){
              this.from_modules(0).ctrl match{
                case ctrl @ (_ : TightlyCoupledIOCtrl| _ : ValidIOCtrl | _: DecoupledIOCtrl)=>{
                  val input_buffer = Module(new InOrderToOutOfOrderQueue(chiselTypeOf(this.in), d.size_of_reorder_buffer))
                  for((str,d) <- this.in.elements){
                    SimpleChiselTransformer.replaceAll(d.ref, input_buffer.out.bits.elements(str), this._commands)
                  }
                  SimpleChiselTransformer.replaceAll(d.in.valid.ref, input_buffer.ctrl.out.valid, this._commands)
                  SimpleChiselTransformer.replaceAll(d.in.ready.ref, input_buffer.ctrl.out.ready, this._commands)
                  SimpleChiselTransformer.replaceAll(d.in.ticket_num.ref, input_buffer.ctrl.out.ticket_num, this._commands)
                  input_buffer.ctrl.in.valid := d.in.valid
                  d.in.ready := input_buffer.ctrl.in.ready
                  this.in <> input_buffer.in.bits
                }
                case ctrl:OutOfOrderIOCtrl => ()
                case _ => ()
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
  def >>> (that: Aggregate): Aggregate = {
      implicit val sourceInfo = UnlocatableSourceInfo
      // val input_ports = that.getElements
      // val output_ports = this.out.getElements
      // if(input_ports.size != output_ports.size){
      //   throwException("The input does not match with outputs")
      // }
      // for((input_port, idx) <- input_ports.zipWithIndex){
      //   input_port.connect(output_ports(idx))(sourceInfo, moduleCompileOptions)
      // }
      this.out >>> that
      that.from_module = Some(this)
      that
  }
}

/** Abstract base class for SimpleChiselState, which only contains basic sequential blocks.
  * These may contain both logic and state which are written in the Module
  * body (constructor).
  * This abstract base class includes an implicit clock and reset.
  *
  * @note SimpleChiselState instantiations must be wrapped in a State() call.
  * @note SimpleChiselState must implement one of the four SimpleChiselIOCtrl as ctrl.
  */
abstract class SimpleChiselState(implicit moduleCompileOptions: CompileOptions)
  extends SimpleChiselModule{}

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
  extends SimpleChiselModule{}