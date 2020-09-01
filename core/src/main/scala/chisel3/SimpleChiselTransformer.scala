package chisel3.simplechisel.internal

import chisel3._
import chisel3.internal._
import chisel3.internal.firrtl._
import chisel3.experimental._
import scala.collection.mutable.ArrayBuffer

object SimpleChiselTransformer {
  def replaceAll(
      lId: Arg,
      rId: Data,
      ctx: ArrayBuffer[Command]
  ): ArrayBuffer[Command] = {
    replaceDsts(lId, rId, ctx)
    replaceUses(lId, rId, ctx)
    return ctx
  }
  //replace the dst lId with rId in ctx and return the new ctx
  def replaceDsts(
      lId: Arg,
      rId: Data,
      ctx: ArrayBuffer[Command]
  ): ArrayBuffer[Command] = {
    for ((cmd, i) <- ctx.zipWithIndex) {
      cmd match {
        case dPrim: DefPrim[_] => {
          val arg_buf = new ArrayBuffer[Arg]
          for (arg <- dPrim.args) {
            arg_buf += arg
          }
          if (dPrim.id.ref.uniqueId.equals(lId.uniqueId))
            ctx.update(
              i,
              DefPrim(dPrim.sourceInfo, rId, dPrim.op, arg_buf.toSeq: _*)
            )
          else
            ctx.update(
              i,
              DefPrim(dPrim.sourceInfo, dPrim.id, dPrim.op, arg_buf.toSeq: _*)
            )
        }
        case connect: Connect => {
          if (connect.loc.uniqueId.equals(lId.uniqueId)) connect.loc = Node(rId)
        }
        case connect: ConnectInit => {
          if (connect.loc.uniqueId.equals(lId.uniqueId)) connect.loc = Node(rId)
        }
        case _ => ()
      }
    }
    return ctx
  }
  //replace all uses of lId with rId in ctx and return the new ctx
  def replaceUses(
      lId: Arg,
      rId: Data,
      ctx: ArrayBuffer[Command]
  ): ArrayBuffer[Command] = {
    for ((cmd, i) <- ctx.zipWithIndex) {
      cmd match {
        case dPrim: DefPrim[_] => {
          val arg_buf = new ArrayBuffer[Arg]
          for (arg <- dPrim.args) {
            if (arg.uniqueId.equals(lId.uniqueId)) {
              arg_buf += rId.ref
            } else {
              arg_buf += arg
            }
          }
          ctx.update(
            i,
            DefPrim(dPrim.sourceInfo, dPrim.id, dPrim.op, arg_buf.toSeq: _*)
          )
        }
        case regInit: DefRegInit => {
          if (regInit.init.uniqueId.equals(lId.uniqueId)) regInit.init = rId.ref
        }
        case whenBegin: WhenBegin => {
          if (whenBegin.pred.uniqueId.equals(lId.uniqueId))
            whenBegin.pred = rId.ref
        }
        case connect: Connect => {
          if (connect.exp.uniqueId.equals(lId.uniqueId)) connect.exp = rId.ref
        }
        case connect: ConnectInit => {
          if (connect.exp.uniqueId.equals(lId.uniqueId)) connect.exp = rId.ref
        }
        case _ => ()
      }
    }
    return ctx
  }

  //replace lId with rId in ctx and return the new ctx
  def replaceWhen(lId: Arg, rId: Arg, ctx: ArrayBuffer[Command])(
      pred: () => Boolean
  ): Option[ArrayBuffer[Command]] = {
    if (pred()) {
      val new_ctx = new ArrayBuffer[Command]
      return Some(new_ctx)
    } else {
      return Some(ctx)
    }
  }

  def removeIfSafe(
      id: Arg,
      ctx: ArrayBuffer[Command]
  ): Option[ArrayBuffer[Command]] = {

    def isDefinition(id: Arg)(command: Command): Boolean = command match {
      case DefWire(_, data) => data.ref.uniqueId.equals(id.uniqueId)
      case DefReg(_, data, clock) =>
        data.ref.uniqueId.equals(id.uniqueId) || clock.uniqueId.equals(
          id.uniqueId
        )
      case DefRegInit(_, data, clock, reset, init) =>
        data.ref.uniqueId.equals(id.uniqueId) || clock.uniqueId.equals(
          id.uniqueId
        ) || reset.uniqueId.equals(id.uniqueId) || init.uniqueId.equals(
          id.uniqueId
        )

      case _ => false
    }

    def dependsOnId(cmd: Command, arg: Command): Boolean = cmd match {
      case DefPrim(sourceInfo, id, _, args) => false
      case DefInvalid(sourceInfo, arg)      => false
    }

    def shouldFilter(cmd: Command): Boolean =
      !ctx.map(c => dependsOnId(c, cmd)).reduce((a, b) => a && b)

    ctx.find(isDefinition(id)) match {
      case None => Some(ctx)
      case Some(definition) =>
        if (shouldFilter(definition))
          Option(ctx.filter(a => a.equals(definition)))
        else Option(ctx)

    }
  }

  // find the most front place to insert the cmd
  def Insert(
      cmd: Command,
      ctx: ArrayBuffer[Command]
  ): Option[ArrayBuffer[Command]] = {
    //TODO
    return None
  }

  // Update functions (Optional)
}
