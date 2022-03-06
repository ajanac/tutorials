package com.netflix

/**
 * @author : ajanasathian
 * @mailto : ajanacs@gmail.com
 * @created : 3/5/22, Saturday
 *          To change this template use File | Settings | File and Code Templates
 *
 *   Need to get back here. Don't understand the solution to this problem
 *
 * */
class StringFilter
{
  final val ONE = 1
  final val TWO = 2

  def stringFilter(inputStr: String): String =
    {
      var state = ONE
      var j = 0
      val str = inputStr.toCharArray
      Range(0, str.length).foreach{i =>
        if (state == ONE && str(i) != 'a' && str(i) != 'b')
          {
            str(j) = str(i)
            j += 1
          }
        if (state == TWO && str(i) != 'c')
          {
            str(j) = 'a'
            j += 1
            if (str(i) != 'a' && str(i) != 'b')
              {
                str(j) = str(i)
                j += 1
              }
            }
        state = if (str(i) == 'a') TWO else ONE
      }
    if (state == TWO)
      {
        str(j) = 'a'
        j += 1
      }
       java.util.Arrays.copyOfRange(str, 0, j).mkString
    }
}
object StringFilter
{
  def main(args: Array[String]): Unit =
    {
      val str1 = "ad"
      val stringFilterInstance = new StringFilter
//      println(s"${stringFilterInstance.stringFilter(str1)}")
      val str2 = "acbac"
      println(s"${stringFilterInstance.stringFilter(str2)}")

    }
}
