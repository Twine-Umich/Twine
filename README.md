# Simple Chisel Specification

Alpha Version 0.1, by Shibo Chen, Updated 7/3/2020

---

## Table of Contents

- [Simple Chisel Specification](#simple-chisel-specification)
  - [Table of Contents](#table-of-contents)
  - [- Bulk Connection](#ullibulk-connectionliul)
  - [Introduction](#introduction)
  - [Installation](#installation)
  - [Abstraction](#abstraction)
  - [Bulk Connection](#bulk-connection)
---

## Introduction

Simple Chisel is a description language at high level. It parses, converts and generates Chisel codes which work as a generator under the hood.

In this specification, we will discuss about the most revolutionary ideas introduced in Simple Chisel first and then introduces optimizations and simplifications we made upon Chisel.

We explore the idea of hardware polymorphism.

## Installation
To install SimpleChisel locally, run the following commands
```shell script
git submodule update --remote
cd simple-chisel
sbt publishLocal
```

## Abstraction
Simple Chisel introduces new abstractions `Logic` and `State`.

All `Logic` modules cannot contain any stateful elements, i.e. `Reg` and `Mem`. Any instance of `Logic` module needs to
be wrapped in `Logic()`

All `State` modules need to contain stateful elements. Any instance of `State` module needs to
be wrapped in `State()`

```scala
class LogicExample extends Logic{
  // Your code here
}

class StateExample extends State{
  // Your code here
}

class Datapth extends Module{
  val logicExample = Logic(new LogicExample)
  val stateExample = State(new StateExample)
}
```

## Bulk Connection

All modules need to implement `in` and `out` as I/O interface to indicate the input and output respectively. Ports in `in` and `out` do not have to be inputs or outputs only, for example, it can be a `ReadyIO` which outputs a `ready` bit. It represents a general idea of the data flow.

Simple Chisel uses a new `>>>` operator to bulk connect between two modules or Bundle.

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

`>>>` operator can be overloaded to all SimpleChisel data or port types.
```
