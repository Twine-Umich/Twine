# Twine Specification

## Recent News

<span style="color:red">Twine has been accepted to DATE 2022!</span>
## Table of Contents

- [Twine Specification](#twine-specification)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Installation](#installation)
  - [Abstraction](#abstraction)
  - [Bulk Connection](#bulk-connection)
  - [Control Interface](#control-interface)
    - [NoIOCtrl](#noioctrl)
    - [TightlyCoupledIOCtrl](#tightlycoupledioctrl)
    - [ValidIOCtrl](#validioctrl)
    - [DecoupledIOCtrl](#decoupledioctrl)
    - [OutOfOrderIOCtrl](#outoforderioctrl)
  - [Mix and Match Rules for Interface Auto-connection](#mix-and-match-rules-for-interface-auto-connection)
    - [Same Level Connection](#same-level-connection)
      - [1-to-1 Connection](#1-to-1-connection)
      - [N-to-1/1-to-N Connection](#n-to-11-to-n-connection)
    - [Cross Level Connection](#cross-level-connection)
  - [Data Type Auto-conversion](#data-type-auto-conversion)
    - [Type Implicit Conversion](#type-implicit-conversion)
    - [Port Width Conversion](#port-width-conversion)
- [Acknowledgement](#acknowledgement)
---

## Introduction

Twine is a hardware design language created to increase the reusability of hardware components and take the heavy-lifting for the designers to make hardware design easy and quick.

In this specification, we will discuss about the major features of SimplChisel with corresponding demos to help developers quickly learn and use Twine.

Twine supports all Chisel designs. In other words, users can integrate their designs in Chisel with Twine seamlessly. All the new features are only supported if the user uses Twine abstractions.

The Twine code-base is synced with Chisel3 newest release periodically to support the most recent Chisel3 features.

## Installation

To install Twine locally, run the following commands

```shell script
git clone https://github.com/Twine-Umich/Twine
cd twine
sbt publishLocal
```

## Abstraction

All Twine features are only available when using Twine abstractions. Therefore, it is important for the module to inherit from Twine classes instead of Chisel3 classes to get access to the Twine features.

Twine introduces new abstractions `TwineModule`, `TwineBaseModule`.

`TwineModule` is the root class for all Twine modules. Any module tends to use Twine features should at least inherit from `TwineModule` if not any other subclasses.


`TwineBaseModule` can be used if the module itself does not follow Twine module requirements but contains Twine modules inside the module.

```scala
class Mod extends TwineModule{
  // Your code here
}

class Datapth extends TwineBaseModule{
  val logicExample = Module(new Mod)
}
```

## Bulk Connection

All modules need to implement `in` and `out` as I/O interface to indicate the input and output respectively. Ports in `in` and `out` do not have to be inputs or outputs only, for example, it can be a `ReadyIO` which outputs a `ready` bit. It represents a general idea of the data flow.

Twine uses a new `>>>` operator to bulk connect between two modules or Bundle. The convention of order is by declaring order instead of port or data names.

```scala
moduleA >>> moduleB
/* This is equivalent to
 foreach( port <- moduleA.out){ // for each port in A's output
   moduleB.in.port := port // Connect it to B's input
 }
*/
bundle >>> module
/* This is equivalent to
 foreach( port <- bundle){ // for each port in bundle
   module.in.port := port // Connect it to module's input
 }
*/

 module >>> bundle
/* This is equivalent to
 foreach( port <- module.out){ // for each port in module's output
   module.in.port := port // Connect it to bundle
 }
*/

`>>>` operator can be overloaded to all Twine data or port types.
```

## Control Interface

In Twine, there are several control interfaces and the designer need to implement one of them for each `TwineModule`.

### NoIOCtrl

`NoIOCtrl` is a very special case where the module is a top-level module therefore it either does not need to implement external control behaviors or it needs to implements its I/O based on the real hardware. It works as a placeholder.

### TightlyCoupledIOCtrl

`TightlyCoupledIOCtrl` is a module designed for the lock-step module that is synced with neignboring modules for each cycle. It has a set of central control signals that tend to minimize the performance and area penalty, but it is the least flexible interface among all the interfaces. The user needs to specify how many cycles of delay the module needs from input to output. The module that implements `TightlyCoupledIOctrl` should be a simple pipelined module that has a fixed delay.

```scala
class TightlyCoupledIO(delay: Int) extends Bundle{
  /* When this signal is raised from the external,
  the pipeline should suspend for the current cycle.*/
  val stall = Input(Bool())


  /* When this signal is raised from the internal,
  it indicates that the neighboring modules would need to either stall for the current cycle or invalidate the outputs from the current module.*/
  val stuck = Output(Bool())
}
```

Here we provide an example that implements a `TightlyCoupledIOCtrl`.

```scala
class TightlyCoupledModule extends TwineModule{

  val in = Input(new Bundle(val d_in = UInt(64.W)))
  val out = Output(new Bundle(val d_out = UInt(64.W)))
  val ctrl = new TightlyCoupledIOCtrl(2) // This is a 2-cycle delay module

  val reg_n = Wire(UInt(64.W))
  val reg_intermediate = RegNext(reg_n)

  when(ctrl.stall){ // what happens if it should stall
    reg_intermediate >>> reg_n
  }
  .otherwise{
      (in.d_in + 1.U) >>> reg_n
  }

  // Suppose d_in === 0.U is a very special case that causes the pipeline to stuck
  Mux(in.d_in === 0.U, true.B, false.B) >>> ctrl.stuck

  // Assign the output
  (reg_intermediate + 1.U) >>> out
}

class Wrapper extends TwineModule{
  val in = ... // We ignore the inputs here since it is just an example
  val out = ... // We ignore the outputs here since it is just an example
  val ctrl = new NoIOCtrl // Since this is the top-level module, we implement NoIOCtrl as the placeholder

  val tightlyCoupledIOModule = Module(new TightlyCoupledModule)

  // An example of how to contrl the module
  false.B >>> tightlyCoupledIOModule.ctrl.stall
  when(tightlyCoupledIOModule.ctrl.stuck){
    // Implement the behaviors when the module is stuck
  }
}
```

### ValidIOCtrl

`ValidIOCtrl` is a module designed for the lock-step module that is synced with neignboring modules for each cycle. Comparing to the `TightlyCoupledIOCtrl`, it implements a pair of `valid` bits at the input and the output. Therefore, `ValidIOCtrl` gives the users more flexibility on how many cycles each request may take.

```scala
class ValidIO extends Bundle{
  /* When this signal is raised from the external,
  the pipeline should suspend for the current cycle.*/
  val stall = Input(Bool())

  /* When this signal is raised from the internal,
  it indicates that the neighboring modules would need to either stall for the current cycle or invalidate the outputs from the current module.*/
  val stuck = Output(Bool())

  /* An input valid port to indicate whether the input data is valid.
  It is raised from the external if the inputs are valid.
  */
  val in = Input(new Bundle{val valid = Bool()})

  /* An output valid port to indicate whether the output data is valid.
  It is raised from the interla if the outputs are valid.
  */
  val out = Output(new Bundle{val valid = Bool()})

}
```

Here we provide an example that implements a `ValidIOCtrl`.

```scala
class ValidIOModule extends TwineModule{

  val in = Input(new Bundle(val d_in = UInt(64.W)))
  val out = Output(new Bundle(val d_out = UInt(64.W)))
  val ctrl = new ValidIOCtrl

  val reg_n = Wire(UInt(64.W))
  val reg_intermediate = RegNext(reg_n)

  val reg_valid_n = Wire(Bool())
  val reg_valid = RegNext(reg_valid_n)

  when(ctrl.stall){ // what happens if it should stall
    reg_intermediate >>> reg_n
    reg_valid >>> reg_valid_n
  }
  .otherwise{
      (in.d_in + 1.U) >>> reg_n
      ctrl.in.valid >>> reg_n
  }

  // Suppose d_in === 0.U is a very special case that causes the pipeline to stuck
  Mux(in.d_in === 0.U, true.B, false.B) >>> ctrl.stuck

  // Assign the output
  (reg_intermediate + 1.U) >>> out
  reg_valid >>> ctrl.out.valid
}

class Wrapper extends TwineModule{
  val in = ... // We ignore the inputs here since it is just an example
  val out = ... // We ignore the outputs here since it is just an example
  val ctrl = new NoIOCtrl // Since this is the top-level module, we implement NoIOCtrl as the placeholder

  val validIOModule = Module(new ValidIOModule)

  // An example of how to contrl the module
  false.B >>> validIOModule.ctrl.stall
  true.B >>> validIOModule.ctrl.in.valid
  when(validIOModule.ctrl.stuck){
    // Implement the behaviors when the module is stuck
  }
  when(!validIOModule.ctrl.out.valid){
    // Implement the behaviors when the outputs is not valid
  }
}
```

### DecoupledIOCtrl

`DecoupledIOCtrl` is a module designed for the decoupled module that accomodates the pressures from the neighboring modules. It implements a set of standard `DecoupledIO` at both ends. We implement the features that generating FIFO buffers automatically at the both ends. The users can specify their desired buffer sizes while generating the ctrl signals. To specify the size of the prepending buffer and the postpending buffer, set `size_of_receiving_buffer` and `size_of_receiving_buffer` during declaration to the number you desire.

If `0` is specified, there is no buffer generated. However, this is greatly discouraged because the purpose of `DecoupledIOCtrl` is to provide a latency insensitive interface to accomodate local pressure.

```scala
class DecoupledIO(val size_of_receiving_buffer: Int, val size_of_receiving_buffer: Int) extends Bundle{

  /* A set of DecoupledIO signals at the input ends
  */
  val in = new Bundle{
    val valid = Input(Bool())
    val ready = Output(Bool())
  })

  /* A set of DecoupledIO signals at the output ends
  */
  val out = new Bundle{
    val valid = Output(Bool())
    val ready = Input(Bool())
  })
}
```

Here we provide an example that implements a `DeoupledIOCtrl`.

```scala
class DecoupledIOModule extends TwineModule{

  val in = Input(new Bundle(val d_in = UInt(64.W)))
  val out = Output(new Bundle(val d_out = UInt(64.W)))
  val ctrl = new DecoupledIOCtrl(4,5) // set the prepending buffer and postpending buffer to 4 and 5 entries respectively

  val reg_n = Wire(UInt(64.W))
  val reg_intermediate = RegNext(reg_n)

  val reg_valid_n = Wire(Bool())
  val reg_valid = RegNext(reg_valid_n)

  when(!ctrl.out.ready){ // When the downstream is not ready for outputs
    reg_intermediate >>> reg_n
    reg_valid >>> reg_valid_n
  }
  .otherwise{
      (in.d_in + 1.U) >>> reg_n
      ctrl.in.valid >>> reg_n
  }

  // Assign the output
  true.B >>> ctrl.in.ready
  reg_valid >>> ctrl.out.valid
  (reg_intermediate + 1.U) >>> out
}

class Wrapper extends TwineModule{
  val in = ... // We ignore the inputs here since it is just an example
  val out = ... // We ignore the outputs here since it is just an example
  val ctrl = new NoIOCtrl // Since this is the top-level module, we implement NoIOCtrl as the placeholder

  val decoupledIOModule = Module(new DecoupledIOModule)

  // An example of how to contrl the module
  true.B >>> decoupledIOModule.ctrl.in.valid
  true.B >>> decoupledIOModule.out.ready
  when(decoupledIOModule.ctrl.in.ready){
    // Implement the behaviors when the module is ready for inputs
  }
  when(!decoupledIOModule.ctrl.out.valid){
    // Implement the behaviors when the outputs is not valid
  }
}
```

### OutOfOrderIOCtrl

`OutOfOrderIOCtrl` provides the most flexible interface to allow the users to handle the requests in an out-of-order manner. The user needs to get a `ticket_num` as the reference while handling each request and
send the `ticket_num` to the output with the valid outputs. We provide the ability to generate re-order buffers at the both ends. The user can set `size_of_reorder_buffer` during declaration.
If `0` is specified, there is no buffer generated. However, this is greatly discouraged because the purpose of `OutOfOrderIOIOCtrl` is to provide a latency insensitive interface to accomodate local pressure.

```scala
class OutOfOrderIOIO(val size_of_reorder_buffer: Int) extends Bundle{

  /* A set of DecoupledIO signals at the input ends
  */
  val in = new Bundle{
    val valid = Input(Bool())
    val ticket_num = Input(log2ceil(size_of_reorder_buffer+1))
    val ready = Output(Bool())
  })

  /* A set of DecoupledIO signals at the output ends
  */
  val out = new Bundle{
    val valid = Output(Bool())
    val ticket_num = Output(log2ceil(size_of_reorder_buffer+1))
    val ready = Input(Bool())
  })
}
```

Here we provide an example that implements a `OutOfOrderIOCtrl`.

```scala
class OutOfOrderIOModule extends TwineModule{

  val in = Input(new Bundle(val d_in = UInt(64.W)))
  val out = Output(new Bundle(val d_out = UInt(64.W)))
  val ctrl = new OutOfOrderIOCtrl(5) // Specify the size of reorder buffer

  val reg_n = Wire(UInt(64.W))
  val reg_intermediate = RegNext(reg_n)

  val reg_valid_n = Wire(Bool())
  val reg_valid = RegNext(reg_valid_n)

  val reg_ticket_num_n = chiselTypeOf(ctrl.in.tick_num)
  val reg_ticket_num = RegNext(reg_ticket_num_n)
 
  when(!ctrl.out.ready){ // When the downstream is not ready for outputs
    reg_intermediate >>> reg_n
    reg_valid >>> reg_valid_n
    reg_ticket_num >>> reg_ticket_num_n
  }
  .otherwise{
      (in.d_in + 1.U) >>> reg_n
      ctrl.in.valid >>> reg_n
      ctrk.in.ticket_in >>> reg_ticket_num
  }

  // Assign the output
  true.B >>> ctrl.in.ready
  reg_valid >>> ctrl.out.valid
  (reg_intermediate + 1.U) >>> out
  reg_ticket_num >>> ctrl.out.ticket_num
}

class Wrapper extends TwineModule{
  val in = ... // We ignore the inputs here since it is just an example
  val out = ... // We ignore the outputs here since it is just an example
  val ctrl = new NoIOCtrl // Since this is the top-level module, we implement NoIOCtrl as the placeholder

  val outOfOrderIOModule = Module(new OutOfOrderIOModule)

  // An example of how to contrl the module
  true.B >>> outOfOrderIOModule.ctrl.in.valid
  true.B >>> outOfOrderIOModule.out.ready

  // Since we don't have OutOfOrderIO on this level, ticket_num would not be connected,
  // It would be ignored.
  // If there is a ticket_num, then ticket_num >>> ctrl.inticket_num
  when(outOfOrderIOModule.ctrl.in.ready){
    // Implement the behaviors when the module is ready for inputs
  }
  when(!outOfOrderIOModule.ctrl.out.valid){
    // Implement the behaviors when the outputs is not valid
  }
}
```

## Mix and Match Rules for Interface Auto-connection

The mix and match rules are rather complex. Therefore we would only provide a intuition on the connection rules here, and check out the [link](#https://umich-my.sharepoint.com/:w:/g/personal/chshibo_umich_edu/ESGIEOPgbLhIjaEocgAmQ4cBmUKOPrLeVsicTp7wJcA30A?e=Kzh7eU) for implementation details.

### Same Level Connection

Same level connections rules for the interconnections between the same level modules.

#### 1-to-1 Connection

When connecting between any `ValidIO` and `TightlyCoupledIO`, since each of them is a lock-step module, all the connecting modules would be connected in a lock-step manner, meaning if one module suspends its pipeline, due to either stall or stuck, the connecting modules would also be paused for the current cycle. However, if the `ValidIO` is at the donwstream, since `ValidIO` has a pair of `valid` bits to keep track of the validity from the upstream, therefore they are not affected by the suspension of upstream modules.

`DecoupledIO` and `OutOfOrderIO` would act independently because of their ability to accomodate local pressure and propogate the pressure through `ready` bit. Notably, `OutOfOrderIO` is the only I/O interface that allows out-of-order execution. To work with other modules, the ordering of the outputs would be restored to the original receiving order if their succeeding module is not an `OutOfOrderIO`.

In order to auto-generate connections between the modules, the modules should be connected with `>>>` operator.

```scala
class Wrapper extends TwineModule{
  val in = ... // We ignore the inputs here since it is just an example
  val out = ... // We ignore the outputs here since it is just an example
  val ctrl = new NoIOCtrl // Since this is the top-level module, we implement NoIOCtrl as the placeholder

  val tightlyCoupledIOModule = Module(new TightlyCoupledIOModule)
  val validIOModule = Module(new ValidIOModule)
  val decoupledIOModule = Module(new DecoupledIOModule)
  val outOfOrderIOModule = Module(new OutOfOrderIOModule)

 // An example of how you can connect multiple modules together
  in >>> tightlyCoupledIOModule >>> validIOModule  >>> decoupledIOModule >>> outOfOrderIOModul >>> out

}
```

#### N-to-1/1-to-N Connection

N-to-1 or 1-to-N is when you have multiple outputs from multiple modules to flow into one module or one module feeds data to multiple downstream modules. A N-to-M case can be reduced into M N-to-1 cases and N 1-to-M cases.

In the N-to-1 case, all bundle and chisel data type would record which module the data coming from and flowing into, therefore preserves the context of topology even though we may not be able to connect two modules directly. All the upstream modules would need to move forward together, thus it equivalently creates a synchronization point between N module and the downstream module.

```scala
class Wrapper extends TwineModule{
  val in = ... // We ignore the inputs here since it is just an example
  val out = ... // We ignore the outputs here since it is just an example
  val ctrl = new NoIOCtrl // Since this is the top-level module, we implement NoIOCtrl as the placeholder

  val tightlyCoupledIOModule = Module(new TightlyCoupledIOModule)
  val validIOModule = Module(new ValidIOModule)
  val decoupledIOModule = Module(new DecoupledIOModule)

  /* The following commands form a 2-to-1 connection
  tightlyCoupledIOModule ----
                             \
                              ------>  DecoupledIO
                             /
  validIOModule ------------
  */
 tightlyCoupledIOModule.out.data >>> decoupledIOModule.in.data1
 validIOModule.out.data >>> decoupledIOModule.in.data2

}
```

N-to-1 is similar thus we would not elaborate further here.

### Cross Level Connection

Cross layer is when you are wrapping multiple `TwineModule`s under one high-level `TwineModule`. You can connect the port of the higher level module to the lower level modules by using `this` or by using the `in` and `out` pair.

## Data Type Auto-conversion

Twine also support automatic type conversions when connecting modules together.

### Type Implicit Conversion

When connection two different types together, Twine would automatically convert one data type to another followed by the rules listed below:
//TODO: Add the table

### Port Width Conversion

It is very common in accelerator designs to parameterize the size of ports and number of functional units, therefore there is a need to serialize and de-serialize data between different functional units. Twine would insert necessary decoupled buffers between the two modules to convert the bandwidth. To utilize this functionality, the designer need to have at least a vector constructor on the one side.

Here are some examples. Notably, in the following examples, all the data type should be part of the port of a `TwineModule`, otherwise there is no control signal support for such conversion.
```scala
val a = Wire(Vec(3, UInt(8.W)))
val b = Wire(Vec(6, UInt(8.W)))
val c = Wire(UInt(8.W))

c >>> b // This will de-serialize the data until we have received 6 units to form a matching large unit
b >>> c // This will serialize the data
a >>> b // Since b is twice as wide as a, this will de-serialize the data stream
b >>> a // Since b is twice as wide as a, this will serialize the data stream
```

### Acknowledgement

This work was supported by the Applications Driving Architectures (ADA) Research Center, a JUMP Center co-sponsored by SRC and DARPA.