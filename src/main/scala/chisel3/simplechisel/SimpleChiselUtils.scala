package chisel3.simplechisel.util


import chisel3._
import chisel3.simplechisel._
import chisel3.simplechisel.util._
import chisel3.experimental.{DataMirror, Direction, requireIsChiselType}
import chisel3.internal.naming._  // can't use chisel3_ version because of compile order
import chisel3.util._

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

  val in = IO(Input(new Bundle{val bits = Vec(entries, gen)}))
  val out = IO(Output(new Bundle{val bits = gen}))
  val ctrl = IO(new DecoupledIOCtrl(0, 0))

  val ram = Mem(entries, genType)
  val deq_ptr = Counter(entries)

  val empty = (deq_ptr.value === 0.U)


  when (ctrl.in.valid && ctrl.in.ready) {
    for(i <- 0 until entries){
      ram(i) := in.bits(i)
    }
  }
  when (ctrl.out.valid && ctrl.out.ready) {
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
  val out = IO(Output(new Bundle{val bits = Vec(entries, gen)}))
  val ctrl = IO(new DecoupledIOCtrl(0, 0))

  val ram = Mem(entries, genType)
  val enq_ptr = Counter(entries)

  val do_enq = ctrl.in.valid && ctrl.in.ready
  val do_deq = ctrl.out.valid && ctrl.out.ready
  
  val empty = (enq_ptr.value === 0.U)
  val full = (enq_ptr.value === (entries - 1).U)

  when (do_enq) {
    ram(enq_ptr.value) := in.bits
    enq_ptr.inc
  }
  when (do_deq) {
    for(i <- 0 until entries){
      out.bits(i) := ram(i)
    }
  }

  ctrl.out.valid := full
  ctrl.in.ready := !empty

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