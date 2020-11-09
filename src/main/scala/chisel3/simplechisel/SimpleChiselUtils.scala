package chisel3.simplechisel.util


import chisel3._
import chisel3.simplechisel._
import chisel3.simplechisel.util._
import chisel3.experimental.{DataMirror, Direction, requireIsChiselType}
import chisel3.internal.naming._  // can't use chisel3_ version because of compile order
import chisel3.util._
import chisel3.simplechisel.hardfloat._
import chisel3.simplechisel.hardfloat.consts._
import scala.collection.mutable.ArrayBuffer

object SimpleChiselTool{

  def expandAndAddElements[T <: Data](simpleChiselBundle: SimpleChiselBundle[T], buffer:ArrayBuffer[Data]): Any ={
    for(elt <- simpleChiselBundle.getElements){
      elt match{
        case rawfloat: RawFloat => {
          buffer += rawfloat
        }
        case aggregate: Aggregate=>{
         expandAndAddElements(aggregate, buffer)
        }
        case _ => {buffer += elt}
      }
    }
  }

  def expandAndAddElements(aggregate: Aggregate, buffer:ArrayBuffer[Data]): Any ={
    aggregate match{
      case rawfloat: RawFloat => {
        buffer += rawfloat
      }
      case _ =>{
        for(elt <- aggregate.getElements){
          elt match{
            case a: Aggregate => expandAndAddElements(a, buffer)
            case _ => buffer += elt
          }
        }
      }
    }
  }

  def morphConnect(sink: Data, source: Data): Data = {
    (sink, source) match{
      case (sink_t: RawFloat, source_t: UInt) =>{
        val converted = rawFloatFromIN(false.B, source_t)
        val resized = resizeRawFloat(sink_t.expWidth, sink_t.sigWidth, converted)
        sink_t <> resized  
      }
      case (sink_t: RawFloat, source_t: SInt) =>{
        val converted = rawFloatFromIN(true.B, source_t)
        val resized = resizeRawFloat(sink_t.expWidth, sink_t.sigWidth, converted)
        sink_t <> resized
      }
      case (sink_t: SInt, source_t: RawFloat) =>{
        val rawToRec = Module(
          new RoundAnyRawFNToRecFN(source_t.expWidth, source_t.sigWidth, source_t.expWidth, source_t.sigWidth,0)
        )
        rawToRec.io.in <> source_t
        rawToRec.io.invalidExc := false.B
        rawToRec.io.infiniteExc := false.B
        rawToRec.io.roundingMode := round_min
        rawToRec.io.detectTininess := false.B
        val recToInt = Module(
          new RecFNToIN(source_t.expWidth, source_t.sigWidth,sink_t.width.get)
        )
        recToInt.io.in := rawToRec.io.out
        recToInt.io.roundingMode := round_min
        recToInt.io.signedOut := true.B
        sink_t := recToInt.io.out
      }
      case (sink_t: UInt, source_t: RawFloat) =>{
        val rawToRec = Module(
          new RoundAnyRawFNToRecFN(source_t.expWidth, source_t.sigWidth, source_t.expWidth, source_t.sigWidth,0)
        )
        rawToRec.io.in <> source_t
        rawToRec.io.invalidExc := false.B
        rawToRec.io.infiniteExc := false.B
        rawToRec.io.roundingMode := round_min
        rawToRec.io.detectTininess := false.B
        val recToInt = Module(
          new RecFNToIN(source_t.expWidth, source_t.sigWidth,sink_t.width.get)
        )
        recToInt.io.in := rawToRec.io.out
        recToInt.io.roundingMode := round_min
        recToInt.io.signedOut := false.B
        sink_t := recToInt.io.out
      }
      case (sink_t: RawFloat, source_t: RawFloat) =>{
        if((sink_t.expWidth + sink_t.sigWidth) == (source_t.expWidth + source_t.sigWidth)){
          sink_t <> source_t
        }else{
          val resized = resizeRawFloat(sink_t.expWidth, sink_t.sigWidth, source_t)
          sink_t <> resized
        }
      }
      case (_,_)=> {
        sink := source
      }
    }
    source
  }
}
/** A hardware module implementing a Serializer
  * @param gen The type of data to queue
  * @param entries The max number of entries in the queue
  *
  */
@chiselName
class Serializer[T <: Data](gen: T,
                       val entries: Int)
                      (implicit compileOptions: chisel3.CompileOptions)
    extends SimpleChiselModule {
  require(entries > -1, "Queue must have non-negative number of entries")
  require(entries != 0, "Use companion object Queue.apply for zero entries")
  val genType =
    if (DataMirror.internal.isSynthesizable(gen)) {
      chiselTypeOf(gen)
    } else {
      gen
    }

  val in = IO(new Bundle{val bits = Input(Vec(entries, genType))})
  val out = IO(new Bundle{val bits = Output(genType)})
  val ctrl = IO(new DecoupledIOCtrl(2, 2))

  val ram = Mem(entries, genType)
  val deq_ptr= Counter(entries)

  val valid = RegInit(false.B)
  val empty = (deq_ptr.value === 0.U) && (!valid)

  when (ctrl.in.valid && ctrl.in.ready) {
    for(i <- 0 until entries){
      ram(i) := in.bits(i)
    }
    valid := true.B
  }
  when (ctrl.out.valid && ctrl.out.ready) {
    valid := (deq_ptr.value != (entries-1).U)
    deq_ptr.inc()
  }

  ctrl.out.valid := !empty
  ctrl.in.ready := empty
  out.bits := ram(deq_ptr.value)

}




/** A hardware module implementing a Parallelizer
  * @param gen The type of data to queue
  * @param entries The max number of entries in the queue
  *
  */
class Parallelizer[T <: Data](gen: T,
                       val entries: Int)
                      (implicit compileOptions: chisel3.CompileOptions)
    extends SimpleChiselModule {
  require(entries > -1, "Queue must have non-negative number of entries")
  require(entries != 0, "Use companion object Queue.apply for zero entries")
  val genType = if (DataMirror.internal.isSynthesizable(gen)) {
      chiselTypeOf(gen)
    } else {
      gen
    }

  val in = IO(new Bundle{val bits = Input(genType)})
  val out = IO(new Bundle{val bits = Output(Vec(entries, genType))})
  val ctrl = IO(new DecoupledIOCtrl(2, 2))

  val ram = Mem(entries, genType)
  val enq_ptr = Counter(entries+1)

  val do_enq = ctrl.in.valid && ctrl.in.ready
  val do_deq = ctrl.out.valid && ctrl.out.ready
  
  val empty = (enq_ptr.value === 0.U)
  val full = (enq_ptr.value === entries.U)

  when (do_enq) {
    ram(enq_ptr.value) := in.bits
    enq_ptr.inc
  }
  when (do_deq){
    enq_ptr.reset
  }
  for(i <- 0 until entries){
    out.bits(i) := ram(i)
  }

  ctrl.out.valid := full
  ctrl.in.ready := !full

}

  
  /** A hardware module implementing an OutOfOrderToInOrderQueue
  * @param gen The type of data to queue
  * @param entries The max number of entries in the queue
  */
class OutOfOrderToInOrderQueue[T <: Data](gen: T,
                       val entries: Int)
                      (implicit compileOptions: chisel3.CompileOptions)
    extends SimpleChiselModule {
  require(entries > -1, "Queue must have non-negative number of entries")
  require(entries != 0, "Use companion object Queue.apply for zero entries")
  val genType = if (compileOptions.declaredTypeMustBeUnbound) {
    requireIsChiselType(gen)
    gen
  } else {
    if (DataMirror.internal.isSynthesizable(gen)) {
      chiselTypeOf(gen)
    } else {
      gen
    }
  }

  val in = IO(Input(new Bundle{val bits = gen}))
  val out = IO(Output(new Bundle{val bits = gen}))
  val ctrl = IO(new OutOfOrderIOCtrl(0))

  val do_enq = ctrl.in.valid && ctrl.in.ready
  val do_deq = ctrl.out.valid && ctrl.out.ready

  val ram = Mem(entries, genType)
  val valid_vec = Vec(entries, Bool())
  val deq_ptr = Counter(entries)

  when (do_enq) {
    ram(ctrl.in.ticket_num) := in.bits
    valid_vec(ctrl.in.ticket_num) := true.B
  }
  when (do_deq) {
    valid_vec(deq_ptr.value) := false.B
    deq_ptr.inc
  }

  out.bits := ram(deq_ptr.value)
  ctrl.out.valid := (valid_vec(deq_ptr.value) === true.B)
  ctrl.in.ready := valid_vec(ctrl.in.ticket_num) === false.B
  ctrl.out.ticket_num := deq_ptr.value
}

/** A hardware module implementing an OutOfOrderToInOrderQueue
  * @param gen The type of data to queue
  * @param entries The max number of entries in the queue
  *
  */
class OutOfOrderToOutOfOrderQueue[T <: Data](gen: T,
                       val entries: Int)
                      (implicit compileOptions: chisel3.CompileOptions)
    extends SimpleChiselModule {
  require(entries > -1, "Queue must have non-negative number of entries")
  require(entries != 0, "Use companion object Queue.apply for zero entries")
  val genType = if (compileOptions.declaredTypeMustBeUnbound) {
    requireIsChiselType(gen)
    gen
  } else {
    if (DataMirror.internal.isSynthesizable(gen)) {
      chiselTypeOf(gen)
    } else {
      gen
    }
  }

  val in = IO(Input(new Bundle{val bits = gen}))
  val out = IO(Output(new Bundle{val bits = gen}))
  val ctrl = IO(new OutOfOrderIOCtrl(0))

  val do_enq = ctrl.in.valid && ctrl.in.ready
  val do_deq = ctrl.out.valid && ctrl.out.ready

  val ram = Mem(entries, genType)
  val valid_vec = Vec(entries, Bool())

  val priorityEncoder = PriorityEncoder(valid_vec)
  when (do_enq) {
    ram(ctrl.in.ticket_num) := in.bits
    valid_vec(ctrl.in.ticket_num) := true.B
  }
  when (do_deq) {
    valid_vec(priorityEncoder) := false.B
  }

  out.bits := ram(priorityEncoder)
  ctrl.out.valid := (PopCount(valid_vec) > 0.U)
  ctrl.in.ready := valid_vec(ctrl.in.ticket_num) === false.B
  ctrl.out.ticket_num := priorityEncoder
}


/** A hardware module implementing an OutOfOrderToInOrderQueue
  * @param gen The type of data to queue
  * @param entries The max number of entries in the queue
  *
  */
class InOrderToOutOfOrderQueue[T <: Data](gen: T,
                       val entries: Int)
                      (implicit compileOptions: chisel3.CompileOptions)
    extends SimpleChiselModule {
  require(entries > -1, "Queue must have non-negative number of entries")
  require(entries != 0, "Use companion object Queue.apply for zero entries")
  val genType = if (compileOptions.declaredTypeMustBeUnbound) {
    requireIsChiselType(gen)
    gen
  } else {
    if (DataMirror.internal.isSynthesizable(gen)) {
      chiselTypeOf(gen)
    } else {
      gen
    }
  }

  val in = IO(Input(new Bundle{val bits = gen}))
  val out = IO(Output(new Bundle{val bits = gen}))
  val ctrl = IO(new OutOfOrderIOCtrl(0))


  val enq_ptr = Counter(entries)
  val do_enq = ctrl.in.valid && ctrl.in.ready
  val do_deq = ctrl.out.valid && ctrl.out.ready

  val ram = Mem(entries, genType)

  val valid_vec = Vec(entries, Bool())

  val priorityEncoder = PriorityEncoder(valid_vec)
  when (do_enq) {
    ram(enq_ptr.value) := in.bits
    enq_ptr.inc
    valid_vec(enq_ptr.value) := true.B
  }
  when (do_deq) {
    valid_vec(priorityEncoder) := false.B
  }

  out.bits := ram(priorityEncoder)
  ctrl.out.valid := (PopCount(valid_vec) > 0.U)
  ctrl.in.ready := valid_vec(enq_ptr.value) === false.B
  ctrl.out.ticket_num := priorityEncoder
}