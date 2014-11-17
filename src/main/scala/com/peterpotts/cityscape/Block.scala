package com.peterpotts.cityscape

case class Block(left: Int, right: Int, height: Int) {
  def contains(value: Int) = left <= value && value <= right

  def fragment(that: Block): List[Block] =
    if (left > that.left) {
      that fragment this
    } else if (that.left > right) {
      // [this this] [that that]
      List(this, that)
    } else if (that.contains(right)) {
      // [this [that this] that]
      val maximum = math.max(height, that.height)
      val intersection = copy(left = that.left, height = maximum)
      List(copy(right = that.left), intersection, that.copy(left = right))
    } else {
      // [this [that that] this]
      val maximum = math.max(height, that.height)
      val intersection = that.copy(height = maximum)
      List(copy(right = that.left), intersection, this.copy(left = that.right))
    }
}
