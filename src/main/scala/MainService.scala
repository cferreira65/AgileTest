/*
 * Copyright (C) 2020 Lightbend Inc. <https://www.lightbend.com>
 */

package docs.http.scaladsl

import akka.actor.typed.ActorSystem

import scala.io.StdIn
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._

import scala.concurrent.{Await, Future}
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{BasicHttpCredentials, OAuth2BearerToken}
import akka.http.scaladsl.unmarshalling.Unmarshal

import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

final case class Item(name: String, id: Long)
final case class Order(items: List[Item])
final case class ApiKey(auth: Boolean, token: String)
final case class Picture(id: String, cropped_picture: String)
final case class Pictures(pictures: List[Picture])

// collect your json format instances into a support trait:
trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val itemFormat = jsonFormat2(Item)
  implicit val orderFormat = jsonFormat1(Order) // contains List[Item]
  implicit val apiKeyFormat = jsonFormat2(ApiKey)
  implicit val pictureFormat = jsonFormat2(Picture)
  implicit val picturesFormat = jsonFormat1(Pictures)
}

object HttpServerRoutingMinimal extends Directives with JsonSupport {
  val basePath = "http://interview.agileengine.com"
  implicit val system = ActorSystem(Behaviors.empty, "my-system")
  // needed for the future flatMap/onComplete in the end
  implicit val executionContext = system.executionContext
  var token = ""

  def main(args: Array[String]): Unit = {

    val responseFuture: Future[HttpResponse] = Http().singleRequest(
      HttpRequest(
        method = HttpMethods.POST,
        uri = basePath + "/auth",
        entity = HttpEntity(ContentTypes.`application/json`, """{ "apiKey": "23567b218376f79d9415" }""")
      )
    )

    responseFuture
      .onComplete {
        case Success(res) => {
          val (res1: Future[ApiKey]) = Unmarshal(res).to[ApiKey]
          res1.foreach(r => token = r.token
          )
        }
        case Failure(_)   => sys.error("something wrong")
      }

    val route = {
      concat (
        path("images") {
          concat(
            get {
              parameters("page".withDefault("1")) { (page) =>
                val authorization = headers.Authorization(OAuth2BearerToken(token))
                val responseFuture: Future[HttpResponse] = Http().singleRequest(
                  HttpRequest(
                    method = HttpMethods.GET,
                    uri = basePath + s"/images?page=${page}",
                    headers = List(authorization),
                  )
                )
                var pictures = Pictures(List(Picture(id = "", cropped_picture = "")))
                responseFuture
                  .onComplete {
                    case Success(res) => {
                      val (res1) = Unmarshal(res).to[Pictures]
                      res1.foreach(pic => pictures = pic)
                    }
                    case Failure(_) => {
                      sys.error("something wrong")
                    }
                  }
                Await.ready(responseFuture, Duration.Inf)
                complete(pictures)
              }
            }
          )
        }
      )
    }

    val bindingFuture = Http().newServerAt("localhost", 8080).bind(route)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }

  def getImages (page: String, token: String) : Pictures = {
    println(token)
    val authorization = headers.Authorization(OAuth2BearerToken(token))
    val responseFuture: Future[HttpResponse] = Http().singleRequest(
      HttpRequest(
        method = HttpMethods.GET,
        uri = basePath + s"/images?page=${page}",
        headers = List(authorization),
      )
    )
    var pictures = Pictures(List(Picture(id = "", cropped_picture = "")))
    Await.ready(responseFuture, Duration.Inf)
    responseFuture
      .onComplete {
        case Success(res) => {
          val (res1) = Unmarshal(res).to[Pictures]
          println(res1)
          res1.foreach(pics => pictures = pics)
        }
        case Failure(_)   => {
          sys.error("something wrong")
        }
      }
    pictures
  }
}