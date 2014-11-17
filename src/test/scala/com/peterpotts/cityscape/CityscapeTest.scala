package com.peterpotts.cityscape

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

import scala.util.Random._

@RunWith(classOf[JUnitRunner])
class CityscapeTest extends WordSpec with Matchers {

  class Target {
    def nextBlock() = {
      val left = nextInt(10)
      val right = left + 1 + nextInt(5)
      val height = 1 + nextInt(10)
      Block(left, right, height)
    }

    val buildings = List(nextBlock(), nextBlock(), nextBlock(), nextBlock(), nextBlock(), nextBlock(), nextBlock())
    val cityscape = Cityscape(buildings)

    implicit class RichListBlock(blocks: List[Block]) {
      def highest(position: Int) = blocks.filter(_ contains position).map(_.height).foldLeft(0)(math.max)
    }

  }

  "A cityscape" should {
    "be as high as the highest building" in {
      new Target {
        val position = nextInt(10)
        var expected = buildings.highest(position)
        var actual = cityscape.highest(position)
        actual should equal(expected)
      }
    }

    "be disjoint ordered blocks" in {
      new Target {
        cityscape.zip(cityscape.tail).forall {
          case (left, right) => left.right <= right.left
        } should equal(true)
      }
    }
  }
}