package chisel3.simplechisel.internal

import chisel3._
import chisel3.internal._
import chisel3.internal.firrtl._
import chisel3.experimental._
import scala.collection.mutable.{ArrayBuffer, ListBuffer, HashMap, Queue}

object SimpleChiselAnalyzer{
    // Find the definition of a node
    def findDef(id: Arg, ctx: Seq[Command]): Option[Command] = {
        for(cmd <- ctx){
            cmd match{
                case c:DefPrim[_] =>{
                    if(c.id.ref.uniqueId.equals(id.uniqueId)){
                        return Some(c)
                    }
                }
                case c:DefWire =>{
                    if(c.id.ref.uniqueId.equals(id.uniqueId)){
                        return Some(c)
                    }
                }
                case c:DefReg =>{
                    if(c.id.ref.uniqueId.equals(id.uniqueId)){
                        return Some(c)
                    }
                }
                case c:DefInstance =>{
                    if(c.id.getRef.uniqueId.equals(id.uniqueId)){
                        return Some(c)
                    }
                }
                case c:DefRegInit =>{
                    if(c.id.ref.uniqueId.equals(id.uniqueId)){
                        return Some(c)
                    }
                }
                case _ => ()
            }
        }
        return None
    }


    def findUses(id: Arg, ctx: ArrayBuffer[Command]): Seq[Command] = {
        val allUsers = new ArrayBuffer[Command]
        for(cmd <- ctx){
            cmd match{
                case c: DefPrim[_] => {
                    for(arg <- c.args){
                        if(arg.uniqueId.equals(id.uniqueId)){
                            allUsers += c
                        }
                    }
                }
                case c:  Connect => {
                    if(c.exp.uniqueId.equals(id.uniqueId)){
                            allUsers += c
                    }
                }
                case c: ConnectInit => {
                    if(c.exp.uniqueId.equals(id.uniqueId)){
                            allUsers += c
                    }
                }
                case c: DefRegInit => {
                    if(c.init.uniqueId.equals(id.uniqueId)){
                            allUsers += c
                    }
                }
            } 
        }
        return allUsers
    }

    def backtracePath(start: BigInt, end: BigInt, parent: HashMap[BigInt, BigInt] ): ListBuffer[BigInt] = {
        val p = parent(end)
        if (p == -1) return ListBuffer(start)

        return (backtracePath(start, p, parent) ++ ListBuffer(end))
    }

    def findPath( graph: HashMap[BigInt, ListBuffer[BigInt]], start: BigInt, end: BigInt): 
                Option[ListBuffer[BigInt]] = {
        var visited: HashMap[BigInt, Boolean] = HashMap()
        var parent: HashMap[BigInt, BigInt] = HashMap()
        val queue = Queue[BigInt]()

        for ((k,v) <- graph) {
            visited += (k -> false)
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

    def traverseData(elt: Data, map:HashMap[BigInt, ListBuffer[BigInt]]):Any = elt match {
        case data: Vec[_] => {
            for(e <- data.getElements){
                if(!map.contains(e.getRef.uniqueId)){
                    map += (e.getRef.uniqueId ->  new ListBuffer[BigInt]())
                }
            }
        }
        case data: Record => {
            for(e <- data.elements.toIndexedSeq){
                traverseData(e._2, map)
            }
        }
        case _ =>{
            if(!map.contains(elt.getRef.uniqueId)){
                map += (elt.getRef.uniqueId -> new ListBuffer[BigInt]())
            }
        }
    }

    def findSameCycleDependencies(id: Arg, ctx: ArrayBuffer[Command]): Seq[BigInt] = {
        val map:HashMap[BigInt, ListBuffer[BigInt]] = new HashMap[BigInt, ListBuffer[BigInt]]
        for(command <- ctx){
          command match{
            case wire:DefWire =>{
              traverseData(wire.id, map)
            }
            case prim:DefPrim[_] =>{              
              traverseData(prim.id, map)
              val id_of_dest = prim.id.getRef.uniqueId
              for(arg <- prim.args){
                if(map.contains(arg.uniqueId)){
                  map(id_of_dest) += arg.uniqueId
                }
              }
            }
            case connect:Connect =>{
                if(map.contains(connect.exp.uniqueId)){
                  if(map.contains(connect.loc.uniqueId)){
                    val id_of_dest = connect.loc.uniqueId
                    map(id_of_dest) += connect.exp.uniqueId
                  }
                }
            }
            case connect:ConnectInit =>{
                if(map.contains(connect.exp.uniqueId)){
                  if(map.contains(connect.loc.uniqueId)){
                    val id_of_dest = connect.loc.uniqueId
                    map(id_of_dest) += connect.exp.uniqueId
                  }
                }
            }
            case connect: BulkConnect => ()
            case _ =>()
          }
        }
        val driving_conditional_args = new ListBuffer[BigInt]()
        for(command <- ctx){
          command match{
            case w: WhenBegin => {driving_conditional_args += w.pred.uniqueId}
            case w: WhenEnd => {if(!w.hasAlt) driving_conditional_args.clear}
            case o: OtherwiseEnd => driving_conditional_args.clear
            case c: ConnectInit => {
              if(map.contains(c.loc.uniqueId)){
                map(c.loc.uniqueId) ++= driving_conditional_args
              }
            }
            case c: Connect => {
              if(map.contains(c.loc.uniqueId)){
                map(c.loc.uniqueId)  ++= driving_conditional_args
              }
            }
            case defP: DefPrim[_] => map(defP.id.getRef.uniqueId)  ++= driving_conditional_args
            case _ =>()
          }
        }
        val allDependencies = new ArrayBuffer[BigInt]

        return findAllDependency(id.uniqueId, allDependencies, map).toSeq
    }
  
    def findAllDependency(id:BigInt, lists:ArrayBuffer[BigInt], 
                      map: HashMap[BigInt, ListBuffer[BigInt]]):ArrayBuffer[BigInt] = {
      if(map.contains(id)){
        for(dependencyId <- map(id)){
          if(!lists.contains(dependencyId)){
            lists += dependencyId
            findAllDependency(dependencyId, lists, map)
          }
        }
      }
      return lists
    }

    def findDependencies(id: Arg, ctx: ArrayBuffer[Command]): Seq[BigInt]  = {
        val map:HashMap[BigInt, ListBuffer[BigInt]] = new HashMap[BigInt, ListBuffer[BigInt]]
        for(command <- ctx){
          command match{
            case wire:DefWire =>{
              traverseData(wire.id, map)
            }
            case reg:DefReg =>{
              traverseData(reg.id, map)
            }
            case reg:DefRegInit =>{
              traverseData(reg.id, map)
              if(map.contains(reg.init.uniqueId)){
                map(reg.id.getRef.uniqueId) += reg.init.uniqueId
              }
            }
            case prim:DefPrim[_] =>{              
              traverseData(prim.id, map)
              val id_of_dest = prim.id.getRef.uniqueId
              for(arg <- prim.args){
                if(map.contains(arg.uniqueId)){
                  map(id_of_dest) += arg.uniqueId
                }
              }
            }
            case connect:Connect =>{
                if(map.contains(connect.exp.uniqueId)){
                  if(map.contains(connect.loc.uniqueId)){
                    val id_of_dest = connect.loc.uniqueId
                    map(id_of_dest) += connect.exp.uniqueId
                  }
                }
            }
            case connect:ConnectInit =>{
                if(map.contains(connect.exp.uniqueId)){
                  if(map.contains(connect.loc.uniqueId)){
                    val id_of_dest = connect.loc.uniqueId
                    map(id_of_dest) += connect.exp.uniqueId
                  }
                }
            }
            case connect: BulkConnect => ()
            case _ =>()
          }
        }
        val driving_conditional_args = new ListBuffer[BigInt]()
        for(command <- ctx){
          command match{
            case w: WhenBegin => {driving_conditional_args += w.pred.uniqueId}
            case w: WhenEnd => {if(!w.hasAlt) driving_conditional_args.clear}
            case o: OtherwiseEnd => driving_conditional_args.clear
            case c: ConnectInit => {
              if(map.contains(c.loc.uniqueId)){
                map(c.loc.uniqueId) ++= driving_conditional_args
              }
            }
            case c: Connect => {
              if(map.contains(c.loc.uniqueId)){
                map(c.loc.uniqueId)  ++= driving_conditional_args
              }
            }
            case defP: DefPrim[_] => map(defP.id.getRef.uniqueId)  ++= driving_conditional_args
            case _ =>()
          }
        }
        val allDependencies = new ArrayBuffer[BigInt]

        return findAllDependency(id.uniqueId, allDependencies, map).toSeq
    }   

    // Whether lId is dependent on rId
    def hasSameCycleDependency(lId: Arg, rId: Arg, ctx: ArrayBuffer[Command]): Boolean  = {
        val map:HashMap[BigInt, ListBuffer[BigInt]] = new HashMap[BigInt, ListBuffer[BigInt]]

        for(command <- ctx){
          command match{
            case wire:DefWire =>{
              traverseData(wire.id, map)
            }
            case prim:DefPrim[_] =>{              
              traverseData(prim.id, map)
              for(arg <- prim.args){
                if(map.contains(arg.uniqueId)){
                  map(arg.uniqueId) += prim.id.getRef.uniqueId
                }
              }
            }
            case connect:Connect =>{
                if(map.contains(connect.exp.uniqueId)){
                  if(map.contains(connect.loc.uniqueId)){
                    map(connect.exp.uniqueId) += connect.loc.uniqueId
                  }
                }
            }
            case connect:ConnectInit =>{
                if(map.contains(connect.exp.uniqueId)){
                  if(map.contains(connect.loc.uniqueId)){
                    map(connect.exp.uniqueId) += connect.loc.uniqueId
                  }
                }
            }
            case connect: BulkConnect => ()
            case _ =>()
          }
        }
        val driving_conditional_args = new ListBuffer[BigInt]()
        for(command <- ctx){
          command match{
            case w: WhenBegin => {
              if(map.contains(w.pred.uniqueId)){
                driving_conditional_args += w.pred.uniqueId
              }
            }
            case w: WhenEnd => {if(!w.hasAlt) driving_conditional_args.clear}
            case o: OtherwiseEnd => driving_conditional_args.clear
            case c: ConnectInit => {
              if(map.contains(c.loc.uniqueId)){
                for(driving_arg <- driving_conditional_args){
                  map(driving_arg) += c.loc.uniqueId
                }
              }
            }
            case c: Connect => {
              if(map.contains(c.loc.uniqueId)){
                for(driving_arg <- driving_conditional_args){
                  map(driving_arg) += c.loc.uniqueId
                }
              }
            }
            case defP: DefPrim[_] =>{
              for(driving_arg <- driving_conditional_args){
                map(driving_arg) += defP.id.getRef.uniqueId
              }
            }
            case _ =>()
          }
        }
        if(map.contains(lId.uniqueId) && map.contains(rId.uniqueId)){
          val result = findPath(map, lId.uniqueId, rId.uniqueId) 
          if(result.isEmpty){
            return false
          }
          else{
            return true
          }
        }else{
          return false
        }
    } 

    // Whether lId is dependent on rId
    def hasDependency(lId: Arg, rId: Arg, ctx: ArrayBuffer[Command]): Boolean = {
        val map:HashMap[BigInt, ListBuffer[BigInt]] = new HashMap[BigInt, ListBuffer[BigInt]]

        for(command <- ctx){
          command match{
            case wire:DefWire =>{
              traverseData(wire.id, map)
            }
            case reg:DefReg =>{
              traverseData(reg.id, map)
            }
            case reg:DefRegInit =>{
              traverseData(reg.id, map)
              if(map.contains(reg.init.uniqueId)){
                map(reg.init.uniqueId) += reg.id.getRef.uniqueId
              }
            }
            case prim:DefPrim[_] =>{              
              traverseData(prim.id, map)
              for(arg <- prim.args){
                if(map.contains(arg.uniqueId)){
                  map(arg.uniqueId) += prim.id.getRef.uniqueId
                }
              }
            }
            case connect:Connect =>{
                if(map.contains(connect.exp.uniqueId)){
                  if(map.contains(connect.loc.uniqueId)){
                    map(connect.exp.uniqueId) += connect.loc.uniqueId
                  }
                }
            }
            case connect:ConnectInit =>{
                if(map.contains(connect.exp.uniqueId)){
                  if(map.contains(connect.loc.uniqueId)){
                    map(connect.exp.uniqueId) += connect.loc.uniqueId
                  }
                }
            }
            case connect: BulkConnect => ()
            case _ =>()
          }
        }
        val driving_conditional_args = new ListBuffer[BigInt]()
        for(command <- ctx){
          command match{
            case w: WhenBegin => {
              if(map.contains(w.pred.uniqueId)){
                driving_conditional_args += w.pred.uniqueId
              }
            }
            case w: WhenEnd => {if(!w.hasAlt) driving_conditional_args.clear}
            case o: OtherwiseEnd => driving_conditional_args.clear
            case c: ConnectInit => {
              if(map.contains(c.loc.uniqueId)){
                for(driving_arg <- driving_conditional_args){
                  map(driving_arg) += c.loc.uniqueId
                }
              }
            }
            case c: Connect => {
              if(map.contains(c.loc.uniqueId)){
                for(driving_arg <- driving_conditional_args){
                  map(driving_arg) += c.loc.uniqueId
                }
              }
            }
            case defP: DefPrim[_] =>{
              for(driving_arg <- driving_conditional_args){
                map(driving_arg) += defP.id.getRef.uniqueId
              }
            }
            case _ =>()
          }
        }
        if(map.contains(lId.uniqueId) && map.contains(rId.uniqueId)){
          val result = findPath(map, lId.uniqueId, rId.uniqueId) 
          if(result.isEmpty){
            return false
          }
          else{
            return true
          }
        }else{
          return false
        }
    }

    def findIndirectDependencies(id: Arg, ctx: ArrayBuffer[Command]): Option[Seq[Command]] = {
        //TODO: Implement this
        return None
    }

    def findDirectDependencies(id: Arg, ctx: ArrayBuffer[Command]): Option[Seq[Command]] = {
        //TODO: Implement this
        return None
    }

}