package se.ramn.concurrent

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import org.scalatest.FunSuite


class FutureTimeoutSuite extends FunSuite {
  test("future that times out should contain an exception") {
    intercept[RuntimeException] {
      Await.result(
        FutureTimeout.raiseWithin(Future { Thread.sleep(1000) }, timeout=500.milliseconds),
        10.seconds)
    }
  }

  test("future that completes should return value") {
    val actual = Await.result(
      FutureTimeout.raiseWithin(Future { Thread.sleep(1000); 123 }, timeout=5000.milliseconds),
      10.seconds)
    assert(actual === 123)
  }
}
