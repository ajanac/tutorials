package com.netflix

/**
 * @author : ajanasathian
 * @mailto : ajanacs@gmail.com
 * @created : 3/5/22, Saturday
 *          To change this template use File | Settings | File and Code Templates
 * */

/*
Flood fill Algorithm
Medium Accuracy: 64.41% Submissions: 17325 Points: 4
An image is represented by a 2-D array of integers, each integer representing the pixel value of the image.

Given a coordinate (sr, sc) representing the starting pixel (row and column) of the flood fill, and a pixel value newColor, "flood fill" the image.

To perform a "flood fill", consider the starting pixel, plus any pixels connected 4-directionally to the starting pixel of the same color as the starting pixel, plus any pixels connected 4-directionally to those pixels (also with the same color as the starting pixel), and so on. Replace the color of all of the aforementioned pixels with the newColor.

Example 1:

Input: image = {{1,1,1},{1,1,0},{1,0,1}},
sr = 1, sc = 1, newColor = 2.
Output: {{2,2,2},{2,2,0},{2,0,1}}
Explanation: From the center of the image
(with position (sr, sc) = (1, 1)), all
pixels connected by a path of the same color
as the starting pixel are colored with the new
color.Note the bottom corner is not colored 2,
because it is not 4-directionally connected to
the starting pixel.


Your Task:
You don't need to read or print anyhting. Your task is to complete the function floodFill() which takes image, sr, sc and newColor as input paramater and returns the image after flood filling.


Expected Time Compelxity: O(n*m)
Expected Space Complexity: O(n*m)
 */

class FloddFil
{
  val M = 8
  val N = 8

  private def floddFilUtil(screen: Array[Array[Int]], x: Int, y: Int, prevC: Int, newC: Int): Unit =
    {
      if (x < 0 || x >= M || y < 0 || y >= N) return
      if(screen(x)(y) != prevC) return
      screen(x)(y) = newC
      floddFilUtil(screen, x  + 1, y, prevC, newC)
      floddFilUtil(screen, x  - 1, y, prevC, newC)
      floddFilUtil(screen, x , y + 1, prevC, newC)
      floddFilUtil(screen, x , y - 1, prevC, newC)
    }

  def floodFill(screen: Array[Array[Int]], x: Int, y: Int, newC: Int): Unit =
    {
      val prevC = screen(x)(y)
      if (prevC == newC) return
      floddFilUtil(screen, x, y, prevC, newC)
    }

  def printMatrix(screen: Array[Array[Int]]): Unit =
    {
      Range(0, M).foreach{i =>
        Range(0, N).foreach{j =>
          print(s"${screen(i)(j)} ")
        }
        println
      }
    }
}
object FloodFil
{
  def main(args: Array[String]): Unit =
    {
      val screen = Array(
        Array(1, 1, 1, 1, 1, 1, 1, 1),
        Array(1, 1, 1, 1, 1, 1, 0, 0),
        Array(1, 0, 0, 1, 1, 0, 1, 1),
        Array(1, 2, 2, 2, 2, 0, 1, 0),
        Array(1, 1, 1, 2, 2, 0, 1, 0),
        Array(1, 1, 1, 2, 2, 2, 2, 0),
        Array(1, 1, 1, 1, 1, 2, 1, 1),
        Array(1, 1, 1, 1, 1, 2, 2, 1))
      val x = 4
      val y = 4
      val newC = 3
      val floddFil = new FloddFil
      floddFil.printMatrix(screen)
      println("********************")
      floddFil.floodFill(screen, x, y, newC)
      floddFil.printMatrix(screen)
    }
}
