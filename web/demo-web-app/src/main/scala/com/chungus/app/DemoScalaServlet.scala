package com.chungus.app

import org.scalatra._

class DemoScalaServlet extends ScalatraServlet {

  get("/") {
    views.html.hello()
  }

  get("/beans") {
    <h1>{params("beans")}</h1>
  }

}
