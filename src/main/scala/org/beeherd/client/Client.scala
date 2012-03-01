/*
 * Filter.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.beeherd.client

/**
 *
 */
trait Client {
  def submit(request: Request): Response
}
