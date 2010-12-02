/*
 * Filter.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.beeherd.dispatcher

/**
 *
 */
trait Dispatcher {
  def dispatch(request: Request): Response
}
