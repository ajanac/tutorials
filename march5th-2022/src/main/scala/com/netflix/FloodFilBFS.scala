package com.netflix

/**
 * @author : ajanasathian
 * @mailto : ajanacs@gmail.com
 * @created : 3/5/22, Saturday
 *          To change this template use File | Settings | File and Code Templates
 * */
class Pair(val first: Int, val second: Int) extends Comparable[Pair]
{
  def compareTo(o: Pair): Int = second - o.second
}
class FloodFilBFS
{
  def validCord(x: Int, y: Int, n: Int, m: Int): Int =
    if (x < 0 || y < 0 || x >= n || y >= m)  0
    else 1

  def bfs(n: Int, m: Int, data: Array[Array[Int]], x: Int, y: Int, color: Int): Unit =
    {
      val vis = Array.fill[Int](101,101)(0)
      val obj = new java.util.LinkedList[Pair]()
      val pq = new Pair(x, y)
      obj.add(pq)
      vis(x)(y) = 1
      while(!obj.isEmpty)
        {
          val coord = obj.peek
          val x1 = coord.first
          val y1 = coord.second
          val preColor = data(x1)(y1)
          data(x1)(y1) = color
          obj.remove()
          if ((validCord(x1 + 1, y1, n, m) == 1) && vis(x1 - 1)(y1) == 0 && data(x1 + 1)(y1) == preColor)
            {
              val p = new Pair(x1 + 1, y1)
              obj.add(p)
              vis(x1 + 1)(y1) = 1
            }
          if ((validCord(x1 - 1, y1, n, m) == 1) && vis(x1 - 1)(y1) == 0 && data(x1 + 1)(y1) == preColor)
            {
              val p = new Pair(x1 - 1, y1)
              obj.add(p)
              vis(x1 - 1)(y1) = 1
            }
          if ((validCord(x1 , y1 + 1, n, m) == 1) && vis(x1 - 1)(y1) == 0 && data(x1 + 1)(y1) == preColor)
            {
              val p = new Pair(x1, y1 + 1)
              obj.add(p)
              vis(x1)(y1 + 1) = 1
            }
          if ((validCord(x1, y1 - 1, n, m) == 1) && vis(x1 - 1)(y1) == 0 && data(x1 + 1)(y1) == preColor)
            {
              val p = new Pair(x1 , y1 - 1)
              obj.add(p)
              vis(x1)(y1 - 1) = 1
            }
         }
    }

  def printMatrix(data: Array[Array[Int]], n: Int, m: Int): Unit =
    {
      Range(0, n).foreach{i =>
        Range(0, m).foreach{j =>print(s"${data(i)(j)} ")}
        println
      }
    }
}
object FloodFilBFS
{
  def main(args: Array[String]): Unit =
    {
      val nn = 8
      val mm = 8
      val data = Array(
        Array( 1, 1, 1, 1, 1, 1, 1, 1 ),
        Array(1, 1, 1, 1, 1, 1, 0, 0 ),
        Array( 1, 0, 0, 1, 1, 0, 1, 1 ),
        Array( 1, 2, 2, 2, 2, 0, 1, 0 ),
        Array( 1, 1, 1, 2, 2, 0, 1, 0 ),
        Array( 1, 1, 1, 2, 2, 2, 2, 0 ),
        Array( 1, 1, 1, 1, 1, 2, 1, 1 ),
        Array( 1, 1, 1, 1, 1, 2, 2, 1 ))
      val xx = 4
      val yy = 4
      val color = 3
      val floodFilBFS = new FloodFilBFS
      floodFilBFS.bfs(nn, mm, data, xx, yy, color)
      floodFilBFS.printMatrix(data, nn, mm)
    }
}
