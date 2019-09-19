package org.combinators.owasp

import org.combinators.cls.types._
import syntax._

object SemanticTypes {
  val servletInstance: Type = Constructor("ServletInstance")
  val responseInstance: Type = Constructor("ResponseInstance")
  val requestInstance: Type = Constructor("RequestInstance")
  val randomSource: Type = Constructor("RandomSource")

  val servletInit: Type = Constructor("ServletInit")
  val randomInit: Type = Constructor("RandomInit")

  val cookieInit: Type = Constructor("CookieInit")
  val cookieMake: Type = Constructor("CookieMake")
  val cookieSet: Type = Constructor("CookieSet")

  val stringCreate: Type = Constructor("StringCreate")
  
  val headersSet: Type = Constructor("HeadersSet")
  val headersList: Type = Constructor("HeadersList")
  val headerSet: Type = Constructor("HeaderSet")

  val paramsSet: Type = Constructor("ParamsSet")
  val paramsList: Type = Constructor("ParamsList")
  val paramSet: Type = Constructor("ParamSet")

  val callCode: Type = Constructor("CallCode")
  val nameCounterProvider: Type = Constructor("NameCounterProvider")
  val cookieList: Type = Constructor("CookieList")
  val randomStringFunction: Type = Constructor("RandomStringFunction")

  val owaspDriver: Type = Constructor("owaspDriver")
}
