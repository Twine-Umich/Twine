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
import scala.collection.mutable.ArrayBuffer

// object MoniteState{
//   def apply[T <: Data](val enable: Bool, val data:T):Any = {

//   }
//   def apply[T <: Data](val enable: Bool, val mem:Mem[T]):Any = {

//   }
// }
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
  override def >>>[T <: SimpleChiselModuleInternal](that: T): T ={
    implicit val sourceInfo = UnlocatableSourceInfo

    if(that.in.getElements == this.out.getElements){
      for((sink_sub, i) <- that.in.getElements.zipWithIndex) {
        (sink_sub, this.out.getElements(i)) match{
          case(sink_vec: Vec[Data @unchecked], source_e: Element) =>{
            val parallelizer = Module(new Parallelizer(source_e, sink_vec.length))
            SimpleChiselTool.morphConnect(parallelizer.in.bits, source_e)
            sink_vec := parallelizer.out.bits
            this.to_modules +=  parallelizer
            parallelizer.from_modules += this
            that.from_modules +=  parallelizer
            parallelizer.to_modules += that
          }
          case(sink_e: Element, source_vec: Vec[Data @unchecked]) =>{
            val serializer = Module(new Serializer(source_vec.sample_element, source_vec.length))
            serializer.in.bits := source_vec
            SimpleChiselTool.morphConnect(sink_e, serializer.out.bits)
            this.to_modules += serializer
            serializer.from_modules += this
            that.from_modules += serializer
            serializer.to_modules += that
          } // Add the transformation cases
          case _ =>{
            sink_sub := this.out.getElements(i)
            if (!this.to_modules.contains(that)) this.to_modules += that
            if(!that.from_modules.contains(this)) that.from_modules += this
          }
        }
      }
    }else{
      val input_ports = new ArrayBuffer[Data]
      val output_ports = new ArrayBuffer[Data]
      SimpleChiselTool.expandAndAddElements(that.in, input_ports)
      SimpleChiselTool.expandAndAddElements(this.out, output_ports)
      for((sink, i) <- input_ports.zipWithIndex){
        SimpleChiselTool.morphConnect(sink,output_ports(i))
      }
      if (!this.to_modules.contains(that)) this.to_modules += that
      if(!that.from_modules.contains(this)) that.from_modules += this
    }
    that
  }

  private[chisel3] def generateSimpleChiselComponent(): Any = {
    val counterVal = Reg(UInt(64.W))
    counterVal := counterVal + 1.U
    this.clock_counter_val = Some(counterVal)
    this.ctrl match{
      case d:TightlyCoupledIOCtrl =>{
        this.debug_input_enable = Some((d.in.valid & (!d.stall) &(!d.stuck) ))
        this.debug_output_enable = Some((d.out.valid & (!d.stall) &(!d.stuck)))
        if(d.delay > 0){
          val tightlyCoupledQ = Reg(Vec(d.delay, Bool()))
          d.out.valid := tightlyCoupledQ(d.delay - 1)
          when( ~(d.stall||d.stuck) ){
            tightlyCoupledQ(0) := d.in.valid
          }
          for(i <- 1 until d.delay){
            when( ~(d.stall||d.stuck)){
              tightlyCoupledQ(i) := tightlyCoupledQ(i-1)
            }
          }
        }else{
          d.out.valid := d.in.valid
        }
      }
      case d: ValidIOCtrl =>{
        this.debug_input_enable = Some((d.in.valid & (!d.stall) &(!d.stuck)))
        this.debug_output_enable = Some((d.out.valid & (!d.stall) &(!d.stuck)))
      }
      case d:DecoupledIOCtrl =>{
        // Only generate the queue if the receiving or sending buffer is not of size 0
        if(d.size_of_receiving_buffer != 0){
          val intype = this.in.cloneType
          for(elt <- intype.getElements){
            elt._specifiedDirection = SpecifiedDirection.Unspecified
          }
          val input_buffer = Module(new Queue(intype, d.size_of_receiving_buffer, true, false))
          for((str,dat) <- this.in.elements){
            SimpleChiselTransformer.replaceUses(dat.ref, input_buffer.io.deq.bits.elements(str), this._commands)
            (dat, input_buffer.io.deq.bits.elements(str)) match{
              case (v_l: Vec[_], v_r: Vec[_]) =>{
                for(i <- 0 until v_l.length){
                  SimpleChiselTransformer.replaceUses(v_l(i).ref, v_r(i), this._commands)
                }
              }

              case _ => ()
            }
          }
          SimpleChiselTransformer.replaceUses(d.in.valid.ref, input_buffer.io.deq.valid, this._commands)
          SimpleChiselTransformer.replaceAll(d.in.ready.ref, input_buffer.io.deq.ready, this._commands)
          input_buffer.io.enq.valid := d.in.valid
          d.in.ready := input_buffer.io.enq.ready
          this.in <> input_buffer.io.enq.bits
        }

        if(d.size_of_sending_buffer != 0){
          val outtype = this.out.cloneType
          for(elt <- outtype.getElements){
            elt._specifiedDirection = SpecifiedDirection.Unspecified
          }
          val output_buffer = Module(new Queue(outtype, d.size_of_sending_buffer, true, false))
          for((str,dat) <- this.out.elements){
            SimpleChiselTransformer.replaceAll(dat.ref, output_buffer.io.enq.bits.elements(str), this._commands)
            (dat, output_buffer.io.enq.bits.elements(str)) match{
              case (v_l: Vec[_], v_r: Vec[_]) =>{
                for(i <- 0 until v_l.length){
                  SimpleChiselTransformer.replaceAll(v_l(i).ref, v_r(i), this._commands)
                }
              }
              case _ => ()
            }
          }
          SimpleChiselTransformer.replaceAll(d.out.valid.ref, output_buffer.io.enq.valid, this._commands)
          SimpleChiselTransformer.replaceUses(d.out.ready.ref, output_buffer.io.enq.ready, this._commands)
          output_buffer.io.deq.ready := d.out.ready
          d.out.valid :=  output_buffer.io.deq.valid
          this.out <> output_buffer.io.deq.bits
        }
        this.debug_input_enable = Some((d.in.valid & d.in.ready))
        this.debug_output_enable = Some((d.out.valid & d.out.ready))
      }
      case d:OutOfOrderIOCtrl =>{
        // Only generate the queue if the re-order buffer size is not of size 0
        if(d.size_of_reorder_buffer != 0){
          var input_uses_inorder = false
          for(m <- this.from_modules){ // We only allow outOfOrder to OutOfOder if it is one-to-one
            var num_of_outOfOrderIO = 0
            m.ctrl match{
              case ctrl @ (_ : TightlyCoupledIOCtrl| _ : ValidIOCtrl | _: DecoupledIOCtrl)=>{
                input_uses_inorder = true
              }
              case ctrl:OutOfOrderIOCtrl =>{
                num_of_outOfOrderIO += 1
                if(num_of_outOfOrderIO >= 2 || m.to_modules.size > 1){
                  input_uses_inorder = true
                }
              }
              case _ =>()
            }
          }

          var output_uses_inorder = false
          for(m <- this.to_modules){ // We only allow outOfOrder to OutOfOder if it is one-to-one
            var num_of_outOfOrderIO = 0
            m.ctrl match{
              case ctrl @ (_ : TightlyCoupledIOCtrl| _ : ValidIOCtrl | _: DecoupledIOCtrl)=>{
                output_uses_inorder = true
              }
              case ctrl:OutOfOrderIOCtrl =>{
                num_of_outOfOrderIO += 1
                if(num_of_outOfOrderIO >= 2 || m.from_modules.size > 1){
                  output_uses_inorder = true
                }
              }
              case _ =>()
            }
          }

          if(input_uses_inorder){
            val input_buffer = Module(new InOrderToOutOfOrderQueue(this.in.cloneType, d.size_of_reorder_buffer))
            for((str,d) <- this.in.elements){
              SimpleChiselTransformer.replaceAll(d.ref, input_buffer.out.elements(str), this._commands)
            }
            SimpleChiselTransformer.replaceAll(d.in.valid.ref, input_buffer.ctrl.out.valid, this._commands)
            SimpleChiselTransformer.replaceAll(d.in.ready.ref, input_buffer.ctrl.out.ready, this._commands)
            SimpleChiselTransformer.replaceAll(d.in.ticket_num.ref, input_buffer.ctrl.out.ticket_num, this._commands)
            input_buffer.ctrl.in.valid := d.in.valid
            d.in.ready := input_buffer.ctrl.in.ready
            this.in <> input_buffer.in
          }
          else{
            val input_buffer = Module(new OutOfOrderToOutOfOrderQueue(this.in.cloneType, d.size_of_reorder_buffer))
            for((str,d) <- this.in.elements){
              SimpleChiselTransformer.replaceAll(d.ref, input_buffer.out.elements(str), this._commands)
            }
            SimpleChiselTransformer.replaceAll(d.in.valid.ref, input_buffer.ctrl.out.valid, this._commands)
            SimpleChiselTransformer.replaceAll(d.in.ready.ref, input_buffer.ctrl.out.ready, this._commands)
            SimpleChiselTransformer.replaceAll(d.in.ticket_num.ref, input_buffer.ctrl.out.ticket_num, this._commands)
            input_buffer.ctrl.in.valid := d.in.valid
            d.in.ready := input_buffer.ctrl.in.ready
            input_buffer.ctrl.in.ticket_num := d.in.ticket_num
            this.in <> input_buffer.in
          }

          if(output_uses_inorder){
            val output_buffer = Module(new OutOfOrderToInOrderQueue(this.in.cloneType, d.size_of_reorder_buffer))
            for((str,d) <- this.out.elements){
              SimpleChiselTransformer.replaceAll(d.ref, output_buffer.in.elements(str), this._commands)
            }
            SimpleChiselTransformer.replaceAll(d.in.valid.ref, output_buffer.ctrl.in.valid, this._commands)
            SimpleChiselTransformer.replaceAll(d.in.ready.ref, output_buffer.ctrl.in.ready, this._commands)
            SimpleChiselTransformer.replaceAll(d.in.ticket_num.ref, output_buffer.ctrl.in.ticket_num, this._commands)
            d.out.valid := output_buffer.ctrl.out.valid
            output_buffer.ctrl.out.ready :=  d.out.ready
            this.out <> output_buffer.out
          }
          else{
            val output_buffer = Module(new OutOfOrderToOutOfOrderQueue(this.in.cloneType, d.size_of_reorder_buffer))
            for((str,d) <- this.in.elements){
              SimpleChiselTransformer.replaceAll(d.ref, output_buffer.in.elements(str), this._commands)
            }
            SimpleChiselTransformer.replaceAll(d.in.valid.ref, output_buffer.ctrl.in.valid, this._commands)
            SimpleChiselTransformer.replaceAll(d.in.ready.ref, output_buffer.ctrl.in.ready, this._commands)
            SimpleChiselTransformer.replaceAll(d.in.ticket_num.ref, output_buffer.ctrl.in.ticket_num, this._commands)
            d.out.valid := output_buffer.ctrl.out.valid
            output_buffer.ctrl.out.ready :=  d.out.ready
            d.out.ticket_num := output_buffer.ctrl.out.ticket_num
            this.out <> output_buffer.out
          }
        }
        this.debug_input_enable = Some((d.in.valid & d.in.ready))
        this.debug_output_enable = Some((d.out.valid & d.out.ready))
      }
      case _ => ()
    }
    val new_commands = new ArrayBuffer[Command]()
    for(cmd <- this._commands){
      cmd match{
        case defInst: DefInstance => new_commands.prepend(defInst)
        case _ => new_commands += cmd
      }
    }
    this._commands = new_commands
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
    
    val generatedComponent = super.generateComponent()
    generatedComponent match{
      case DefModule(_, m_name, m_ports, m_commands) =>{
        val new_commands = m_commands.to[ArrayBuffer]
        if(Builder.enableDebugging){
          if(!this.debug_input_enable.isEmpty)
            new_commands += WhenBegin(UnlocatableSourceInfo, this.debug_input_enable.get.ref)
            new_commands += Printf(UnlocatableSourceInfo, this.clock.getRef, p"Cycle: ${this.clock_counter_val.getOrElse(0.U)} Module:${this.name} inputs\n")
          for(elt <- this.in.getElements){
            elt match{
              case b:Bits => new_commands += Printf(UnlocatableSourceInfo, this.clock.getRef, p"${b.getRef.name}=0x${Hexadecimal(b)}\n")
              case _ => new_commands += Printf(UnlocatableSourceInfo, this.clock.getRef, p"${elt.getRef.name}=${elt}\n")
            }
          }
          if(!this.debug_input_enable.isEmpty)
            new_commands += WhenEnd(UnlocatableSourceInfo, 0)
          if(!this.debug_output_enable.isEmpty)
            new_commands += WhenBegin(UnlocatableSourceInfo, this.debug_output_enable.get.ref)
          new_commands += Printf(UnlocatableSourceInfo, this.clock.getRef, p"Cycle: ${this.clock_counter_val.getOrElse(0.U)} Module:${this.name} outputs\n")
          for(elt <- this.out.getElements){
            elt match{
              case b:Bits => new_commands += Printf(UnlocatableSourceInfo, this.clock.getRef, p"${b.getRef.name}=0x${Hexadecimal(b)}\n")
              case _ => new_commands += Printf(UnlocatableSourceInfo, this.clock.getRef, p"${elt.getRef.name}=${elt}\n")
            }              }
          if(!this.debug_output_enable.isEmpty)
            new_commands += WhenEnd(UnlocatableSourceInfo, 0)
        }
        DefModule(this,m_name, m_ports, new_commands.toSeq)
      }
      case _ => generatedComponent
    }
  }

  /** Connect this to that $coll mono-directionally hand side and element-wise.
    *
    * @param that the $coll to connect to
    * @group Connect
    */
  def >>> (that: Aggregate): Aggregate = {
    implicit val sourceInfo = UnlocatableSourceInfo
    this.out >>> that
    if(that._parent.isDefined){
      that._parent.get match{
        case sm: SimpleChiselModuleInternal =>{
          if (!sm.from_modules.contains(this)) sm.from_modules += this
          if(!this.to_modules.contains(sm)) this.to_modules += sm
          if(sm.sub_modules.contains(this)){
            if (!that.from_modules.contains(this) ) that.from_modules += this
            for(elt <- that.getElements)
              if (!elt.from_modules.contains(this)) elt.from_modules += this
          }
        }
        case _ =>()
      }
    }
    that
  }

  def >>> [T <: Data](that: SimpleChiselBundle[T]): SimpleChiselBundle[T] = {
    if(that.getElements.size == this.out.getElements.size){
      for((sink_sub, i) <- that.getElements.zipWithIndex) {
        (sink_sub, this.out.getElements(i)) match{
          case(sink_vec: Vec[Data @unchecked], source_e: Element) =>{
            val parallelizer = Module(new Parallelizer(source_e, sink_vec.length))
            SimpleChiselTool.morphConnect(parallelizer.in.bits, source_e)
            sink_vec := parallelizer.out.bits
            this.to_modules +=  parallelizer
            parallelizer.from_modules += this
            if(sink_sub._parent.isDefined){
              sink_sub._parent.get match{
                case sm:SimpleChiselModuleInternal =>{
                  if(!sm.sub_modules.contains(this)){
                    sm.from_modules += parallelizer
                    parallelizer.to_modules += sm
                  }else{
                    sink_sub.from_modules += parallelizer
                  }
                }
                case _ =>()
              }
            }
          }
          case(sink_e: Element, source_vec: Vec[Data @unchecked]) =>{
            val serializer = Module(new Serializer(source_vec.sample_element, source_vec.length))
            serializer.in.bits := source_vec
            SimpleChiselTool.morphConnect(sink_e, serializer.out.bits)
            this.to_modules += serializer
            serializer.from_modules += this
            if(sink_sub._parent.isDefined){
              sink_sub._parent.get match{
                case sm:SimpleChiselModuleInternal =>{
                  if(!sm.sub_modules.contains(this)){
                    sm.from_modules += serializer
                    serializer.to_modules += sm
                  }else{
                    sink_sub.from_modules += serializer
                  }
                }
                case _ =>()
              }
            }
          } // Add the transformation cases
          case _ =>{
            SimpleChiselTool.morphConnect(sink_sub, this.out.getElements(i))
            if(sink_sub._parent.isDefined){
              sink_sub._parent.get match{
                case sm:SimpleChiselModuleInternal =>{
                  if(!sm.sub_modules.contains(this)){
                    if(!this.to_modules.contains(sm)) this.to_modules += sm
                    if(!sm.from_modules.contains(this)) sm.from_modules += this
                  }else{
                    if(!sink_sub.from_modules.contains(this)) sink_sub.from_modules += this
                  }
                }
                case _ =>()
              }
            }
          }
        }
      }
    }else{
      val input_ports = new ArrayBuffer[Data]
      val output_ports = new ArrayBuffer[Data]
      SimpleChiselTool.expandAndAddElements(that, input_ports)
      SimpleChiselTool.expandAndAddElements(this.out, output_ports)
      for((sink, i) <- input_ports.zipWithIndex){
        SimpleChiselTool.morphConnect(sink,output_ports(i))
        if(sink._parent.isDefined){
          sink._parent.get match{
            case sm:SimpleChiselModuleInternal =>{
              if(!sm.sub_modules.contains(this)){
                if(!this.to_modules.contains(sm)) this.to_modules += sm
                if(!sm.from_modules.contains(this)) sm.from_modules += this
              }else{
                if(!sink.from_modules.contains(this)) sink.from_modules += this
              }
            }
            case _ =>()
          }
        }
      }
    }
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