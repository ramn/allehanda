package se.ramn.concurrent

import java.util.concurrent.Executors
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration.Duration


object FutureTimeout {
  private val scheduledExecutor: ScheduledExecutorService =
    Executors.newScheduledThreadPool(1)

  def raiseWithin[T](future: Future[T], timeout: Duration): Future[T] = {
    raiseWithin(future, timeout.toMillis)
  }

  def raiseWithin[T](future: Future[T], timeoutMillis: Long): Future[T] = {
    val promise = Promise[T]()
    promise tryCompleteWith future
    val failAfterTimeout = asRunnable {
      promise.tryFailure {
        new RuntimeException(s"Future timed out after $timeoutMillis ms")
      }
    }
    scheduledExecutor.schedule(failAfterTimeout, timeoutMillis, TimeUnit.MILLISECONDS)
    promise.future
  }

  private def asRunnable[T](f: => T): Runnable =
    new Runnable { override def run: Unit = { f; () } }
}
