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

    def isDefinition(id: Arg, eql: Arg => Boolean, eql2: Data => Boolean)(
        command: Command
    ): Boolean =
      command match {
        case DefPrim(_, _, _, args @ _*)  => args.forall(a => eql(a))
        case DefInvalid(_, arg)           => eql(arg)
        case DefWire(_, data)             => eql2(data)
        case DefReg(_, data, _)           => eql2(data)
        case DefRegInit(_, data, _, _, _) => eql2(data)
        case _                            => false
      }

    def dependsOnId(cmd: Command, eql: Arg => Boolean): Boolean = cmd match {
      case Connect(_, _, exp)     => eql(exp)
      case WhenBegin(_, pred)     => eql(pred)
      case ConnectInit(_, _, exp) => eql(exp)
      case Stop(_, clock, _)      => eql(clock)
      case Printf(_, clock, _)    => eql(clock)
      case _                      => false
    }

    def arg_equality(a: Arg) = a.uniqueId.equals(id.uniqueId)
    ctx.find(
      isDefinition(
        id,
        arg_equality,
        b => b.ref.uniqueId.equals(id.uniqueId)
      )
    ) match {
      case None => Some(ctx)
      case Some(definition) =>
        if (!ctx.forall(c => dependsOnId(c, arg_equality)))
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
