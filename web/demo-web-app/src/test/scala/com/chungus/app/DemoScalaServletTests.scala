package com.chungus.app

import org.scalatra.test.scalatest._

class DemoScalaServletTests extends ScalatraFunSuite {

  addServlet(classOf[DemoScalaServlet], "/*")

  test("GET / on DemoScalaServlet should return status 200") {
    get("/") {
      status should equal (200)
    }
  }

}
