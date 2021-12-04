
package chisel3.twine.util

import chisel3._
import chisel3.twine._
import chisel3.internal.requireIsChiselType
import scala.collection.immutable.ListMap
import scala.collection.mutable.ArrayBuffer


final class TwineBundle[T <: Data](private val eltsIn: Seq[T]) extends collection.IndexedSeq[T] {

  // Clone the inputs so that we have our own references.
  private val elts: IndexedSeq[T] = eltsIn.toIndexedSeq

  /**
    * Statically (elaboration-time) retrieve the element at the given index.
    * @param index Index with which to retrieve.
    * @return Retrieved index.
    */
  def apply(index: Int): T = elts(index)

  /** Strong bulk connect, assigning elements in this TwineBundle from elements in a Seq.
    *
    * @note the lengths of this and that must match
    */
  def :=(that: Seq[T]): Unit = {
    require(this.length == that.length)
    for ((a, b) <- this zip that)
      a := b
  }

  def length: Int = elts.length

  val elements = ListMap(elts.zipWithIndex.map { case (element, index) => (index.toString, element) }: _*)

  def getElements: Seq[Data] =
    (0 until length).map(apply(_))


  def >>> (that: Aggregate): Aggregate = {
    val input_ports = new ArrayBuffer[Data]
    val output_ports = this.getElements
    TwineTool.expandAndAddElements(that, input_ports)

    for((input_port, idx) <- input_ports.zipWithIndex){
      input_port := output_ports(idx)
      for(m<- input_port.from_modules){
        if(!that.from_modules.contains(m)) that.from_modules+= m
      }
      if(output_ports(idx)._parent.isDefined && that._parent.isDefined){
        if(output_ports(idx)._parent.get._id != that._parent.get._id){
          output_ports(idx)._parent.get match{
            case sm:TwineModuleInternal =>{
              if(!that.from_modules.contains(sm)) that.from_modules += sm
            }
            case _ =>()
          }
        }
      }
    }
    that
  }

  def >>> [T <: Data](that: TwineBundle[T]): TwineBundle[T] = {
    val input_ports = that.getElements
    val output_ports = this.getElements

    for((input_port, idx) <- input_ports.zipWithIndex){
      input_port := output_ports(idx)
    }
    that
  }

  def >>>[T <: TwineModule](that: T): T = {
    if(this.getElements.size == that.in.getElements.size){
      for((source_sub, i) <- this.getElements.zipWithIndex) {
        (that.in.getElements(i), source_sub) match{
          case(sink_vec: Vec[Data @unchecked], source_e: Element) =>{
            if(sink_vec.length > 1){
              val parallelizer = Module(new Parallelizer(source_e, sink_vec.length))
              TwineTool.morphConnect(parallelizer.in.bits, source_e)
              sink_vec := parallelizer.out.bits
              that.from_modules +=  parallelizer
              parallelizer.to_modules += that
              if(source_e._parent.isDefined){
                source_e._parent.get match{
                  case sm:TwineModuleInternal =>{
                    if(!sm.sub_modules.contains(that)){
                      sm.to_modules += parallelizer
                      parallelizer.from_modules += sm
                    }else{
                      source_e.to_modules += parallelizer
                    }
                  }
                  case _ =>()
                }
              }
            }else{
              TwineTool.morphConnect(sink_vec(0), source_sub)
              if(source_sub._parent.isDefined){
                source_sub._parent.get match{
                  case sm:TwineModuleInternal =>{
                    if(!sm.sub_modules.contains(that)){
                      if(!that.from_modules.contains(sm)) that.from_modules += sm
                      if(!sm.to_modules.contains(that)) sm.to_modules += that
                    }else{
                      if(!source_sub.to_modules.contains(that)) source_sub.to_modules += that
                    }
                  }
                  case _ =>()
                }
              }
            }
          }
          case(sink_e: Element, source_vec: Vec[Data @unchecked]) =>{
            if(source_vec.length > 1){
              val serializer = Module(new Serializer(source_vec.sample_element, source_vec.length))
              serializer.in.bits := source_vec
              TwineTool.morphConnect(sink_e, serializer.out.bits)
              that.from_modules += serializer
              serializer.to_modules += that
              if(source_sub._parent.isDefined){
                source_sub._parent.get match{
                  case sm:TwineModuleInternal =>{
                    if(!sm.sub_modules.contains(that)){
                      sm.to_modules += serializer
                      serializer.from_modules += sm
                    }else{
                      source_sub.to_modules += serializer
                    }
                  }
                  case _ =>()
                }
              }
            }
          } // Add the transformation cases
          case _ =>{
            TwineTool.morphConnect(that.in.getElements(i), source_sub)
            if(source_sub._parent.isDefined){
              source_sub._parent.get match{
                case sm:TwineModuleInternal =>{
                  if(!sm.sub_modules.contains(that)){
                    if(!that.from_modules.contains(sm)) that.from_modules += sm
                    if(!sm.to_modules.contains(that)) sm.to_modules += that
                  }else{
                    if(!source_sub.to_modules.contains(that)) source_sub.to_modules += that
                  }
                }
                case _ =>()
              }
            }
          }
        }
      }
    }else{
      this >>> that.in
      for(elt <-this.getElements){
        if(elt._parent.isDefined){
          elt._parent.get match{
            case sm:TwineModuleInternal =>{
              if(!that.from_modules.contains(sm)&& (!sm.sub_modules.contains(that))) that.from_modules += sm
              if(!sm.to_modules.contains(that) && (!sm.sub_modules.contains(that))) 
                sm.to_modules += that

              if(sm.sub_modules.contains(that)){
                if (!elt.to_modules.contains(that) ) 
                  elt.to_modules += that
              }
            }
            case _ =>()
          }
        }
      }
    }

    that
  }
}

object TwineBundle{

  def apply[T <: Data](eltsIn: Seq[T]): TwineBundle[T] = new TwineBundle(eltsIn)

  def apply[T <: Data](val0: T, vals: T*): TwineBundle[T] = new TwineBundle(val0 +: vals.toSeq)

  def apply[T <: Data](simpleChiselBundle: TwineBundle[T]): TwineBundle[T] = new TwineBundle(simpleChiselBundle.elts)

  /**
    * Create a TwineBundle type from the type of the given Vec.
    *
    * @example {{{
    * TwineBundle(Vec(2, UInt(8.W))) = TwineBundle(Seq.fill(2){UInt(8.W)})
    * }}}
    */
  def apply[T <: Data](vec: Vec[T]): TwineBundle[T] = {
    TwineBundle(Seq.fill(vec.length)(vec.sample_element))
  }
}