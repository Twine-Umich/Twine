
/*============================================================================

This Chisel source file is part of a pre-release version of the HardFloat IEEE
Floating-Point Arithmetic Package, by John R. Hauser (with some contributions
from Yunsup Lee and Andrew Waterman, mainly concerning testing).

Copyright 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017 The Regents of the
University of California.  All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice,
    this list of conditions, and the following disclaimer.

 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions, and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 3. Neither the name of the University nor the names of its contributors may
    be used to endorse or promote products derived from this software without
    specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS "AS IS", AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, ARE
DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=============================================================================*/

package chisel3.twine.hardfloat

import Chisel._

import chisel3.dontTouch

class Equiv_MulAddRecFN(expWidth: Int, sigWidth: Int) extends Module
{
    val io = new Bundle {
        val a = Bits(INPUT, expWidth + sigWidth + 1)
        val b = Bits(INPUT, expWidth + sigWidth + 1)
        val c = Bits(INPUT, expWidth + sigWidth + 1)
        val op = Bits(INPUT, 3)
        val roundingMode   = UInt(INPUT, 3)
        val detectTininess = UInt(INPUT, 1)
        val isGoodA = Bits(OUTPUT, 1)
        val isGoodB = Bits(OUTPUT, 1)
        val isGoodC = Bits(OUTPUT, 1)
        val isGoodRecFN = Bits(OUTPUT, 1)

        val out = Bits(OUTPUT, expWidth + sigWidth)
        val exceptionFlags = Bits(OUTPUT, 5)
    }

    val mulAddRecFN = Module(new MulAddRecFN(expWidth, sigWidth))
    mulAddRecFN.io.op := io.op
    mulAddRecFN.io.a := io.a
    mulAddRecFN.io.b := io.b
    mulAddRecFN.io.c := io.c
    mulAddRecFN.io.roundingMode   := io.roundingMode
    mulAddRecFN.io.detectTininess := io.detectTininess

    io.out := fNFromRecFN(expWidth, sigWidth, mulAddRecFN.io.out)
    io.exceptionFlags := mulAddRecFN.io.exceptionFlags
    io.isGoodA := isGoodRecFN(expWidth, sigWidth, io.a)
    io.isGoodB := isGoodRecFN(expWidth, sigWidth, io.b)
    io.isGoodC := isGoodRecFN(expWidth, sigWidth, io.c)
    io.isGoodRecFN := isGoodRecFN(expWidth, sigWidth, mulAddRecFN.io.out)
    dontTouch(io.isGoodA)
    dontTouch(io.isGoodB)
    dontTouch(io.isGoodC)
    dontTouch(io.isGoodRecFN)
}

class Equiv_MulAddRecF16 extends Equiv_MulAddRecFN(5, 11)
class Equiv_MulAddRecF32 extends Equiv_MulAddRecFN(8, 24)
class Equiv_MulAddRecF64 extends Equiv_MulAddRecFN(11, 53)
