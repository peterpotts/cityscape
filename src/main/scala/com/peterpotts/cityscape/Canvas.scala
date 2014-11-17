package com.peterpotts.cityscape

object Canvas {
  def draw(blocks: List[Block]): List[Line] = {
    val points = trace(blocks)

    points.zip(points.tail).map {
      case (from, to) => Line(from, to)
    }
  }

  def trace(blocks: List[Block]): List[Point] = blocks match {
    case head :: tail =>
      Point(head.left, 0) :: Point(head.left, head.height) :: Point(head.right, head.height) :: trace(head, tail)
    case Nil => Nil
  }

  def trace(head: Block, body: List[Block]): List[Point] = body match {
    case thorax :: tail =>
      val points = Point(thorax.left, thorax.height) :: Point(thorax.right, thorax.height) :: trace(thorax, tail)
      if (head.right < thorax.left) Point(head.right, 0) :: Point(thorax.left, 0) :: points else points
    case Nil =>
      Point(head.right, 0) :: Nil
  }
}

case class Point(x: Int, y: Int)

case class Line(from: Point, to: Point)