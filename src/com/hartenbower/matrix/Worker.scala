package com.hartenbower.matrix

import scala.actors.Actor
import org.apache.log4j.Logger

case class NewChunk[T](s: Scheduler[T], chunk: Tuple2[Int, Int])
case class FinishedChunk(m: Worker)
case object Stop
case class Init[T](action: Tuple2[Int, Int] => Unit)
object Scheduler {
  val DEBUG = false
  val log = Logger.getLogger(classOf[Scheduler[_]])
}
import Scheduler.{ log, DEBUG }

class Scheduler[T](val queue: Array[T], val elementSize: Int, val procs: Int = Runtime.getRuntime.availableProcessors) extends Actor {
  val total = queue.size / elementSize
  var workers: Set[Worker] = Set[Worker]()
  var lastIdx = 0
  var finished = false
  val factor = 2
  val chunkSize = total / procs

  // tuple is index range into backing array
  def init(action: Tuple2[Int, Int] => Unit) = {
    finished = false
    lastIdx = 0
    if (DEBUG) log.info("total " + total)
    for (i <- 1 to procs; if lastIdx < total) {
      val m = new Worker("worker" + i, action)
      m.start
      workers += m
      val chunk = (lastIdx, if (lastIdx + chunkSize > total - 1) total - 1 else lastIdx + chunkSize)
      lastIdx = chunk._2 + 1
      m ! NewChunk(this, chunk)
    }
    if (DEBUG) log.info("created " + workers.size + " workers")
  }

  def nextChunk(): Option[Tuple2[Int, Int]] = {
    if (lastIdx > total - 1) {
      None
    } else if (lastIdx + chunkSize > total - 1) {
      val ret = (lastIdx, total - 1)
      lastIdx = total
      Option(ret)
    } else {
      val ret = (lastIdx, lastIdx + chunkSize)
      lastIdx += chunkSize + 1
      Option(ret)
    }
  }

  def act {
    react {
      case Init(action) =>
        init(action)
        act
      case FinishedChunk(m) =>
        if (DEBUG) log.info(m + " finished chunk asks for more ")
        nextChunk() match {
          case Some(t) =>
            if (DEBUG) log.info("giving " + m + " " + (t._2 - t._1) + " more recs")
            m ! NewChunk(this, t)
            act
          case _ =>
            if (DEBUG) log.info("queue is empty! workers " + workers + ", m " + m)
            workers -= m
            m ! Stop
            if (workers.isEmpty) {
              prolog
              finished = true
            } else {
              act
            }
        }
      case msg =>
        println("Unhandled message: " + msg)
    }
  }

  def prolog() {
    if (DEBUG) log.info("prolog")
  }

}

class Worker(name: String, action: Tuple2[Int, Int] => Unit) extends Actor {
  var count = 0
  def act() {
    react {
      case NewChunk(s, chunk) =>
        if (DEBUG) log.info(this + " in " + Thread.currentThread.getName + " received new chunk " + chunk)
        val results = action(chunk)
        count += chunk._2 - chunk._1
        s ! FinishedChunk(this)
        act
      case Stop =>
        if (DEBUG) log.info(this + " is stopping")
      case msg =>
        println("Unhandled message: " + msg)
    }
  }

  override def toString = name + " :: [" + count + "]"
}


