package app

import intervalTree.IntervalTree
import scala.collection.JavaConverters._

/**
  * Created by nkatz on 18/12/19.
  */
package object data {

  implicit class ITree[T](val tree: IntervalTree[T]) {

    def +=(from: Long, to: Long, data: T): Unit = tree.addInterval(from, to, data)

    def range(from: Long, to: Long): List[(Long, Long, T)] = {
      tree.getIntervals(from, to).asScala.toList.map { i =>
        if (from < i.getStart && to > i.getStart) (i.getStart, to, i.getData)
        else if (from >= i.getStart && to > i.getEnd) (from, i.getEnd, i.getData)
        else (from, to, i.getData)
      }
    }

  }

}
