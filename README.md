# Simple Chisel Specification

Alpha Version 0.1, by Shibo Chen, Updated 7/3/2020

Chisel/FIRRTL development meetings happen every Monday and Tuesday from 1100--1200 PT.

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
<<<<<<< HEAD
=======

### Design Verification

These simulation-based verification tools are available for Chisel:
- [**iotesters**](https://github.com/freechipsproject/chisel-testers), specifically [PeekPokeTester](https://github.com/freechipsproject/chisel-testers/wiki/Using%20the%20PeekPokeTester), provides constructs (`peek`, `poke`, `expect`) similar to a non-synthesizable Verilog testbench.
- [**testers2**](https://github.com/ucb-bar/chisel-testers2) is an in-development replacement for PeekPokeTester, providing the same base constructs but with a streamlined interface and concurrency support with `fork` and `join`.


## Documentation

### Useful Resources

- [**Cheat Sheet**](https://github.com/freechipsproject/chisel-cheatsheet/releases/latest/download/chisel_cheatsheet.pdf), a 2-page reference of the base Chisel syntax and libraries
- [**Wiki**](https://github.com/freechipsproject/chisel3/wiki), which contains various feature-specific tutorials and frequently-asked questions.
- [**ScalaDoc**](https://www.chisel-lang.org/api/latest/chisel3/index.html), a listing, description, and examples of the functionality exposed by Chisel
- [**Gitter**](https://gitter.im/freechipsproject/chisel3), where you can ask questions or discuss anything Chisel
- [**Website**](https://www.chisel-lang.org) ([source](https://github.com/freechipsproject/www.chisel-lang.org/))

If you are migrating from Chisel2, see [the migration guide](https://www.chisel-lang.org/chisel3/chisel3-vs-chisel2.html).

### Data Types Overview
These are the base data types for defining circuit components:

![Image](https://raw.githubusercontent.com/freechipsproject/chisel3/master/doc/images/type_hierarchy.svg?sanitize=true)

## Developer Documentation
This section describes how to get started developing Chisel itself, including how to test your version locally against other projects that pull in Chisel using [sbt's managed dependencies](https://www.scala-sbt.org/1.x/docs/Library-Dependencies.html).

### Compiling and Testing Chisel

First, clone and build the master branch of [FIRRTL](https://github.com/freechipsproject/firrtl) and [Treadle](https://github.com/freechipsproject/treadle), as the master branch of Chisel may depend on unreleased changes in those projects:

```
git clone https://github.com/freechipsproject/firrtl.git
git clone https://github.com/freechipsproject/treadle.git
pushd firrtl; sbt publishLocal; popd
pushd treadle; sbt publishLocal; popd
```

Clone and build the Chisel library:

```
git clone https://github.com/freechipsproject/chisel3.git
cd chisel3
sbt compile
```

If the compilation succeeded, you can then run the included unit tests by invoking:

```
sbt test
```

### Running Projects Against Local Chisel

To use the development version of Chisel (`master` branch), you will need to build from source and `publishLocal`.
The repository version can be found in the build.sbt file.
As of the time of writing it was:

```
version := "3.2-SNAPSHOT"
```

To publish your version of Chisel to the local Ivy (sbt's dependency manager) repository, run:

```
sbt publishLocal
```

The compiled version gets placed in `~/.ivy2/local/edu.berkeley.cs/`.
If you need to un-publish your local copy of Chisel, remove the directory generated in `~/.ivy2/local/edu.berkeley.cs/`.

In order to have your projects use this version of Chisel, you should update the `libraryDependencies` setting in your project's build.sbt file to:

```
libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.2-SNAPSHOT"
```

### Chisel3 Architecture Overview

The Chisel3 compiler consists of these main parts:

- **The frontend**, `chisel3.*`, which is the publicly visible "API" of Chisel
  and what is used in Chisel RTL. These just add data to the...
- **The Builder**, `chisel3.internal.Builder`, which maintains global state
  (like the currently open Module) and contains commands, generating...
- **The intermediate data structures**, `chisel3.firrtl.*`, which are
  syntactically very similar to Firrtl. Once the entire circuit has been
  elaborated, the top-level object (a `Circuit`) is then passed to...
- **The Firrtl emitter**, `chisel3.firrtl.Emitter`, which turns the
  intermediate data structures into a string that can be written out into a
  Firrtl file for further processing.

Also included is:
- **The standard library** of circuit generators, `chisel3.util.*`. These
  contain commonly used interfaces and constructors (like `Decoupled`, which
  wraps a signal with a ready-valid pair) as well as fully parameterizable
  circuit generators (like arbiters and multiplexors).
- **Chisel Stage**, `chisel3.stage.*`, which contains compilation and test
  functions that are invoked in the standard Verilog generation and simulation
  testing infrastructure. These can also be used as part of custom flows.

### Which version should I use?

The chisel eco-system (`chisel3`, `firttl`, `dsptools`, `firrtl-interpreter`, `treadle`, `diagrammer`) use a form of semantic versioning:
 major versions are identified by two leading numbers, separated by a dot (i.e., `3.2`), minor versions by a single number following the major version, separated by a dot.
 We maintain API compatibility within a major version (i.e., `3.2.12` should be API-compatible with `3.2.0`), but do not guarantee API compatibility between major versions
 (i.e., APIs may change between `3.1.8` and `3.2.0`).
 We may introduce new definitions or add additional parameters to existing definitions in a minor release, but we do our best to maintain compatibility with previous minor releases of a major release - code that worked in `3.2.0` should continue to work un-modified in `3.2.10`.

We encourage chisel users (rather than chisel developers), to use release versions of chisel.
 The chisel web site (and GitHub repository) should indicate the current release version.
 If you encounter an issue with a released version of chisel, please file an issue on GitHub mentioning the chisel version and provide a simple test case (if possible).
 Try to reproduce the issue with the associated latest minor release (to verify that the issue hasn't been addressed).

If you're developing a chisel library (or `chisel` itself), you'll probably want to work closer to the tip of the development trunk.
 By default, the master branches of the chisel repositories are configured to build and publish their version of the code as `Z.Y-SNAPSHOT`.
 We try to publish an updated SNAPSHOT every two weeks.
 There is no guarantee of API compatibility between SNAPSHOT versions, but we publish date-stamped `Z.Y-yyyymmdd-SNAPSHOT` versions which will not change.
 The code in `Z.Y-SNAPSHOT` should match the code in the most recent `Z.Y-yyyymmdd-SNAPSHOT` version, the differences being the chisel library dependencies:
 `Z.Y-SNAPSHOT`s depend on `V.U-SNAPSHOT`s and `Z.Y-yyyymmdd-SNAPSHOT`s will depend on `V.U-yyyymmdd-SNAPSHOT`s.
 **NOTE**: Prior to the `v3.2-20191030-SNAPSHOT` version, we used `Z.Y-mmddyy-SNAPSHOT` to tag and name published SNAPSHOTs.

If you're developing a library (or another chisel tool), you should probably work with date-stamped SNAPSHOTs until your library or tool is ready to be published (to ensure a consistent API).
 Prior to publishing, you should verify your code against generic (no date-stamp) SNAPSHOTs, or locally published clones of the current master branches of chisel dependencies.
>>>>>>> chisel3/3.4-release
