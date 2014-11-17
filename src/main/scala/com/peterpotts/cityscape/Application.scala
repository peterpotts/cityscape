package com.peterpotts.cityscape

object Application {
  def main(args: Array[String]): Unit = {
    val buildings = List(Block(2, 6, 2), Block(1, 3, 1), Block(4, 5, 3))
    val cityscape = Cityscape(buildings)
    println("*** Buildings ***")
    buildings.foreach(println)
    println("*** Cityscape ***")
    cityscape.foreach(println)
    println("*** Lines ***")
    Canvas.draw(cityscape).foreach(println)
  }
}