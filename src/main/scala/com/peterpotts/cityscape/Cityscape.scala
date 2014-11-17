package com.peterpotts.cityscape

import scala.annotation.tailrec

object Cityscape {
  implicit val blockOrdering = Ordering.by[Block, Int](_.left)

  def apply(buildings: List[Block]): List[Block] = merge(shred(buildings.sorted))

  private def shred(blocks: List[Block]): List[Block] = shred(Vector.empty, blocks)

  private def merge(blocks: List[Block]): List[Block] = merge(Nil, blocks)

  @tailrec private def shred(shredded: Vector[Block], blocks: List[Block]): List[Block] = blocks match {
    case first :: second :: tail =>
      val sum = first fragment second
      val (left, right) = sum.partition(_.right <= second.left)
      shred(shredded ++ left, (right ++ tail).sorted)
    case _ =>
      (shredded ++ blocks).toList
  }

  @tailrec private def merge(merged: List[Block], blocks: List[Block]): List[Block] = blocks match {
    case first :: second :: tail if first.height == second.height =>
      merge(merged, first.copy(right = second.right) :: tail)
    case head :: tail if head.left == head.right =>
      merge(merged, tail)
    case head :: tail =>
      merge(head :: merged, tail)
    case Nil =>
      merged.reverse
  }
}
