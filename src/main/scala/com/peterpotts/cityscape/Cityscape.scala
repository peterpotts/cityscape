package com.peterpotts.cityscape

import scala.annotation.tailrec

object Cityscape {
  implicit val blockOrdering = Ordering.by[Block, Int](_.left)

  def apply(buildings: List[Block]): List[Block] = merge(shred(buildings.sorted))

  private def shred(blocks: List[Block]): List[Block] = shred(Vector.empty, blocks)

  private def merge(blocks: List[Block]): List[Block] = merge(Nil, blocks)

  @tailrec private def shred(shredded: Vector[Block], blocks: List[Block]): List[Block] = blocks match {
    case head :: thorax :: tail =>
      val sum = head fragment thorax
      val (left, right) = sum.partition(_.right <= thorax.left)
      shred(shredded ++ left, sortedZip(right, tail))
    case _ => (shredded ++ blocks).toList
  }

  @tailrec private def merge(merged: List[Block], blocks: List[Block]): List[Block] = blocks match {
    case head :: thorax :: tail if head.height == thorax.height =>
      merge(merged, head.copy(right = thorax.right) :: tail)
    case head :: tail if head.left == head.right => merge(merged, tail)
    case head :: tail => merge(head :: merged, tail)
    case Nil => merged.reverse
  }

  private def sortedZip(left: List[Block], right: List[Block]): List[Block] = (left, right) match {
    case (leftHead :: leftTail, rightHead :: rightTail) =>
      if (leftHead.left < rightHead.left)
        leftHead :: sortedZip(leftTail, right)
      else
        rightHead :: sortedZip(left, rightTail)
    case (_, Nil) => left
    case (Nil, _) => right
  }
}
