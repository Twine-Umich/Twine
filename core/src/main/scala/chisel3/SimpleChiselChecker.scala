package chisel3.simplechisel

import scala.collection.mutable.{ListBuffer, HashMap, StringBuilder, ArrayBuffer}
import collection.mutable.Queue
import chisel3.internal._
import chisel3.internal.Builder._
import chisel3.internal.firrtl._
import chisel3.simplechisel._
import chisel3.internal.firrtl.PrimOp._
import chisel3._

object SimpleChiselChecker{

    val lists: ListBuffer[ListBuffer[Int]] = new ListBuffer[ListBuffer[Int]]
    val hashMap:HashMap[String, Int] = new HashMap[String, Int]

    // Take a name and add the name into the map space
    def addName(name: String): String = {
        if(!hashMap.contains(name)){
            hashMap += (name -> lists.size)
            lists += new ListBuffer[Int]()
        }
        name
    }

    def traverseData(elt: Data, d: Component):Any = elt match {
        case data: Vec[_] => {
            for(e <- data.getElements){
                if(!hashMap.contains(s"${e.getRef.fullName(d)}")){
                    hashMap += (s"${e.getRef.fullName(d)}" -> lists.size)
                    lists += new ListBuffer[Int]()
                }
            }
        }
        case data: Record => {
            for(e <- data.elements.toIndexedSeq){
                traverseData(e._2, d)
            }
        }
        case _ =>{
            if(!hashMap.contains(s"${elt.getRef.fullName(d)}")){
                hashMap += (s"${elt.getRef.fullName(d)}" -> lists.size)
                lists += new ListBuffer[Int]()
            }
        }
    }

    def backtracePath(start: Int, end: Int, parent: Map[Int, Int] ): ListBuffer[Int] = {
        val p = parent(end)
        if (p == -1) return ListBuffer(start)

        return (backtracePath(start, p, parent) ++ ListBuffer(end))
    }

    def findPath( graph: ListBuffer[ListBuffer[Int]], start: Int, end: Int): Option[ListBuffer[Int]] = {
        var visited: Map[Int, Boolean] = Map()
        var parent: Map[Int, Int] = Map()
        val queue = Queue[Int]()

        for (i <- 0 to graph.length) {
            visited += (i -> false)
        }
        queue.enqueue(start)
        parent += (start -> -1)

        while (queue.nonEmpty) {
            val node = queue.dequeue()
            if (node == end) return Some(backtracePath(start, end, parent))
            if (!visited(node)) {
                visited += (node -> true)
                for (neighbor <- graph(node)) {
                    parent += (neighbor -> node)
                    queue.enqueue(neighbor)
                }

            }
        }

        return None
    }

    def simpleChiselCtrlCheck(component: Component): Component = {
        hashMap.clear
        lists.clear
        component match{
        case d:DefModule =>{
            var pos_of_valid_in = 0
            var pos_of_valid_out = 0
            var pos_of_ready_in = 0
            var pos_of_ready_out = 0
            var continue = false
            for(port <- d.ports){
                port.id match {
                    case ctrl: DecoupledIOCtrlInternal =>{
                        continue = true
                        hashMap += (s"${ctrl.in.valid.getRef.fullName(d)}" -> lists.size)
                        pos_of_valid_in = lists.size
                        lists += new ListBuffer[Int]()
                        hashMap += (s"${ctrl.in.ready.getRef.fullName(d)}" -> lists.size)
                        pos_of_ready_in = lists.size
                        lists += new ListBuffer[Int]()
                        hashMap += (s"${ctrl.out.valid.getRef.fullName(d)}" -> lists.size)
                        pos_of_valid_out = lists.size
                        lists += new ListBuffer[Int]()
                        hashMap += (s"${ctrl.out.ready.getRef.fullName(d)}" -> lists.size)
                        pos_of_ready_out = lists.size
                        lists += new ListBuffer[Int]()
                    }
                    case ctrl: OutOfOrderIOCtrlInternal =>{
                        continue = true
                        hashMap += (s"${ctrl.in.valid.getRef.fullName(d)}" -> lists.size)
                        pos_of_valid_in = lists.size
                        lists += new ListBuffer[Int]()
                        hashMap += (s"${ctrl.in.ready.getRef.fullName(d)}" -> lists.size)
                        pos_of_ready_in = lists.size
                        lists += new ListBuffer[Int]()
                        hashMap += (s"${ctrl.out.valid.getRef.fullName(d)}" -> lists.size)
                        pos_of_valid_out = lists.size
                        lists += new ListBuffer[Int]()
                        hashMap += (s"${ctrl.out.ready.getRef.fullName(d)}" -> lists.size)
                        pos_of_ready_out = lists.size
                        lists += new ListBuffer[Int]()
                        hashMap += (s"${ctrl.in.ticket_num.getRef.fullName(d)}" -> lists.size)
                        lists += new ListBuffer[Int]()
                        hashMap += (s"${ctrl.out.ticket_num.getRef.fullName(d)}" -> lists.size)
                        lists += new ListBuffer[Int]()
                    }
                    case _ => traverseData(port.id, d)
                }
            }
        if(!continue){
          return component
        }
        for(command <- d.commands){
          command match{
            case wire:DefWire =>{
              traverseData(wire.id, d)
            }
            case _ =>()
          }
        }
        for(command <- d.commands){
          command match{
            case prim:DefPrim[_] =>{              
              traverseData(prim.id, d)
              val idx_of_dest = hashMap(prim.id.getRef.fullName(d))
              for(arg <- prim.args){
                if(hashMap.contains(arg.fullName(d))){
                  lists(hashMap(arg.fullName(d))) += idx_of_dest
                }
              }
            }
            case _ =>()
          }
        }
        for(command <- d.commands){
          command match{
            case connect:Connect =>{
                if(hashMap.contains(connect.exp.fullName(d))){
                  if(hashMap.contains(connect.loc.fullName(d))){
                    val idx_of_dest = hashMap(connect.loc.fullName(d))
                    lists(hashMap(connect.exp.fullName(d))) += idx_of_dest
                  }
                }
            }
            case connect:ConnectInit =>{
                if(hashMap.contains(connect.exp.fullName(d))){
                  if(hashMap.contains(connect.loc.fullName(d))){
                    val idx_of_dest = hashMap(connect.loc.fullName(d))
                    lists(hashMap(connect.exp.fullName(d))) += idx_of_dest
                  }
                }
            }
            case connect: BulkConnect => ()
            case _ =>()
          }
        }
        val driving_conditional_args = new ListBuffer[Int]()
        for(command <- d.commands){
          command match{
            case w: WhenBegin => {driving_conditional_args += hashMap(w.pred.fullName(d))}
            case w: WhenEnd => {if(!w.hasAlt) driving_conditional_args.clear}
            case o: OtherwiseEnd => driving_conditional_args.clear
            case c: ConnectInit => {
              if(hashMap.contains(c.loc.fullName(d))){
                lists(hashMap(c.loc.fullName(d))) ++= driving_conditional_args
              }
            }
            case c: Connect => {
              if(hashMap.contains(c.loc.fullName(d))){
                lists(hashMap(c.loc.fullName(d)))  ++= driving_conditional_args
              }
            }
            case defP: DefPrim[_] => lists(hashMap(defP.id.getRef.fullName(d)))  ++= driving_conditional_args
            case _ =>()
          }
        }

        findPath(lists, pos_of_valid_in,pos_of_ready_in) match{
          case Some(list) =>{
            //TODO: change to throw expection
            Console.println("Violation found")
          }
          case None =>()
        }
        findPath(lists, pos_of_ready_out,pos_of_valid_out) match{
          case Some(list)=>{
            //TODO: change to throw expection
            Console.println("Violation found")
          }
          case None =>()
        }
      }
    }
    component
  }
}