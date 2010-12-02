package org.beeherd.http

import scala.collection.mutable.ListBuffer

class Calculator {
  private val hist = new ListBuffer[String]

  def add(left: Int, right: Int): Int = {
    (left + " + " + right) +=: hist
    left + right
  }

  def history = hist.toList
}
