package org.combinators.owasp

import java.nio.file.Paths

import com.github.javaparser.ast.expr.Expression
import org.combinators.cls.interpreter.{ReflectedRepository, combinator}
import org.combinators.templating.twirl.Java
import SemanticTypes._
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.stmt.{BlockStmt, Statement}
import com.github.javaparser.ast.Modifier
import org.combinators.cls.git.{EmptyInhabitationBatchJobResults, ResultLocation, Results}
import org.combinators.cls.types._
import org.combinators.templating.persistable.JavaPersistable._
import org.combinators.templating.persistable.Persistable
import syntax._


class Repository(driverModel: OWASPDriverModel) {

  @combinator object InitCode {
    def apply(servletInstance: Expression, responseInstance: Expression, requestInstance: Expression): Seq[Statement] = {
      Java(
        s"""
           |javax.servlet.http.HttpServlet ${servletInstance} = new org.owasp.benchmark.testcode.${driverModel.benchmarkName}();
           |HttpServletRequest ${requestInstance} = new HttpServletRequest();
           |HttpServletResponse ${responseInstance} = new HttpServletResponse();
           |""".stripMargin
      ).statements()
    }
    val semanticType: Type =
      servletInstance =>: responseInstance =>: requestInstance =>: servletInit
  }


  @combinator object RequestServletInstance {
    def apply: Expression =
      Java("servlet").expression()

    val semanticType: Type = servletInstance
  }

  @combinator object ResponseInstance {
    def apply: Expression =
      Java("response").expression()

    val semanticType: Type = responseInstance
  }

  @combinator object RequestInstance {
    def apply: Expression =
      Java("request").expression()

    val semanticType: Type = requestInstance
  }


  @combinator object RandomInitialization {
    def apply(randomSource: Expression): Seq[Statement] = {
      if (driverModel.needsRandom) {
        Java(
          s"""
             |Random ${randomSource} = new Random();
             |""".stripMargin
        ).statements()
      } else Seq.empty
    }

    val semanticType: Type = randomSource =>: randomInit
  }

  @combinator object RandomSource {
    def apply: Expression =
      Java("r").expression()
    val semanticType: Type = randomSource
  }

  @combinator object PerformRequest {
    def apply(servletInstance: Expression, responseInstance: Expression, requestInstance: Expression): Seq[Statement] = {
      Java(
        s"""
           |HttpServletCaller caller = new HttpServletCaller(${servletInstance});
           |caller.callPost((javax.servlet.http.HttpServletRequest)${requestInstance}, (javax.servlet.http.HttpServletResponse) ${responseInstance});
           |""".stripMargin
      ).statements()
    }
    val semanticType: Type =
      servletInstance =>: responseInstance =>: requestInstance =>: callCode
  }

  @combinator object CookieInitCode {
    def apply(cookieList: Expression): Seq[Statement] =
      driverModel.cookieProvider match {
        case NoCookie => Seq.empty
        case _ =>
          Java(
            s"""
               |ArrayList<Cookie> ${cookieList} = new ArrayList<>();
               |""".stripMargin
          ).statements()
      }

    val semanticType: Type = cookieList =>: cookieInit
  }

  @combinator object CreateCookie {
    def apply(randomFun: RandomStringFun, nameCounter: () => Int, cookieList: Expression): Seq[Statement] = {
      def toCode(cookie: CookieProvider): Seq[Statement] =
        cookie match {
          case NoCookie => Seq.empty
          case RandomCookie(length) =>
            val (nextCookieName, nextCookieNameInst) = randomFun("cookieName", length)
            val (nextCookieValue, nextCookieValueInst) = randomFun("cookieValue", length)
            val nextCookieInstance = s"cookieInstance${nameCounter()}"
            Java(
              s"""
                 |${nextCookieName.mkString("\n")}
                 |${nextCookieValue.mkString("\n")}
                 |Cookie $nextCookieInstance = new Cookie($nextCookieNameInst, $nextCookieValueInst);
                 |${cookieList}.add($nextCookieInstance);
                 |""".stripMargin).statements()
          case FixedCookie(name, value) =>
            val nextCookieInstance = s"cookieInstance${nameCounter()}"
            Java(
              s"""
                 |Cookie $nextCookieInstance = new Cookie($name, $value);
                 |${cookieList}.add($nextCookieInstance);
                 |""".stripMargin).statements()
          case MultipleCookies(cookies) => cookies.flatMap(toCode)
        }

      toCode(driverModel.cookieProvider)
    }
    val semanticType = randomStringFunction =>: nameCounterProvider =>: cookieList =>: cookieMake
  }

  @combinator object SetCookieCode {
    def apply(cookieList: Expression, requestInstance: Expression): Seq[Statement] =
      driverModel.cookieProvider match {
        case NoCookie => Seq.empty
        case _ =>
          Java(
            s"""
               |${requestInstance}.setCookies(${cookieList}.toArray(new Cookie[] {}));
               |""".stripMargin
          ).statements()
      }

    val semanticType: Type = cookieList =>: requestInstance =>: cookieSet
  }

  @combinator object NextName {
    var nameCounter = 0
    def apply: () => Int = ()  => {
      nameCounter += 1
      nameCounter
    }
    val semanticType: Type = nameCounterProvider
  }

  @combinator object CookieList {
    def apply: Expression = Java("cookies").expression()
    val semanticType: Type = cookieList
  }

  @combinator object HeadersList {
    def apply: Expression = Java("headers").expression()
    val semanticType: Type = headersList
  }

  @combinator object ParamList {
    def apply: Expression = Java("parameters").expression()
    val semanticType: Type = paramsList
  }

  type RandomStringFun = (String, Option[Int]) => (Seq[Statement], Expression)
  @combinator object CreateRandomString {
    def apply(randomInstance: Expression,
      nameCounter: () => Int): RandomStringFun =
      (name, length) => {
        val lengthCode =
          length match {
            case None => ""
            case Some(l) => s", $l"
          }
        val lengthSuffix =
          length match {
            case None => ""
            case Some(_) => "Length"
          }
        val counter = nameCounter()
        (Java(s"String $name${counter} = createSymbString${lengthSuffix}($randomInstance$lengthCode);").statements(),
          Java(s"$name${counter}").expression())
      }

    val semanticType = randomSource =>: nameCounterProvider =>: randomStringFunction
  }

  type CreateStringFun = (String, StringValue) => (Seq[Statement], Expression)
  @combinator object CreateStringValue {
    def apply(randomStringFun: RandomStringFun): CreateStringFun = {
      case (name, RandomStringValue(l)) => randomStringFun(name, l)
      case (_, FixedStringValue(value)) => (Seq.empty, Java(s""""$value"""").expression())
    }
    val semanticType: Type = randomStringFunction =>: stringCreate
  }

  @combinator object SetHeaders {
    def apply(requestInstance: Expression, createHeader: CreateStringFun, headerList: Expression): Seq[Statement] = {
      def toCode(header: StringValue): Seq[Statement] = {
        val (headerCode, headerInst) = createHeader("header", header)
        Java(
          s"""
             |${headerCode.mkString("\n")}
             |${headerList}.add($headerInst);
             |""".stripMargin).statements()
      }

      driverModel.headerProvider match {
        case StandardHeaderProvider(_, Some(headers)) =>
          Java(
            s"""
               |ArrayList<String> ${headerList} = new ArrayList<>();
               |${headers.flatMap(toCode).mkString("\n")}
               |${requestInstance}.setHeaders(${headerList}.toArray(new String[] {}));
               |""".stripMargin).statements()
        case StandardHeaderProvider(_, None) =>Seq.empty
      }
    }
    val semanticType = requestInstance =>: stringCreate =>: headersList =>: headersSet
  }

  @combinator object SetHeader {
    def apply(requestInstance: Expression, createHeader: CreateStringFun): Seq[Statement] = {
      driverModel.headerProvider match {
        case StandardHeaderProvider(Some(header), _) =>
          val (headerCode, headerInst) = createHeader("header", header)
          Java(
            s"""
               |${headerCode.mkString("\n")}
               |${requestInstance}.setHeader(${headerInst});
               |""".stripMargin).statements()
        case StandardHeaderProvider(None, _) => Seq.empty
      }
    }
    val semanticType = requestInstance =>: stringCreate =>: headerSet
  }


  @combinator object SetParameter {
    def apply(requestInstance: Expression, createParam: CreateStringFun): Seq[Statement] = {
      driverModel.requestProvider match {
        case StandardParameterProvider(Some(param), _) =>
          val (paramCode, paramInst) = createParam("parameter", param)
          Java(
            s"""
               |${paramCode.mkString("\n")}
               |${requestInstance}.setParameter(${paramInst});
               |""".stripMargin).statements()
        case StandardParameterProvider(None, _) => Seq.empty
      }
    }
    val semanticType = requestInstance =>: stringCreate =>: paramSet
  }

  @combinator object SetParameters {
    def apply(requestInstance: Expression, createParameter: CreateStringFun, parametersList: Expression): Seq[Statement] = {
      def toCode(param: StringValue): Seq[Statement] = {
        val (paramCode, paramInst) = createParameter("parameter", param)
        Java(
          s"""
             |${paramCode.mkString("\n")}
             |${parametersList}.add($paramInst);
             |""".stripMargin).statements()
      }

      driverModel.requestProvider match {
        case StandardParameterProvider(_, Some((paramKey, paramVals))) =>
          val (paramKeyCode, paramKeyInst) = createParameter("parameterKey", paramKey)
          Java(
            s"""
               |ArrayList<String> ${parametersList} = new ArrayList<>();
               |${paramVals.flatMap(toCode(_)).mkString("\n")}
               |
               |${requestInstance}.setParameterMap($paramKeyInst, ${parametersList}.toArray(new String[] {}));
               |""".stripMargin).statements()
        case StandardParameterProvider(_, None) => Seq.empty
      }
    }
    val semanticType = requestInstance =>: stringCreate =>: paramsList =>: paramsSet
  }

  @combinator object MakeDriver {
    def apply(
      initCode: Seq[Statement],
      randomInit: Seq[Statement],
      cookieInit: Seq[Statement],
      cookieList: Seq[Statement],
      cookieSet: Seq[Statement],
      headers: Seq[Statement],
      header: Seq[Statement],
      param: Seq[Statement],
      paramMap: Seq[Statement],
      sendCode: Seq[Statement]
    ): CompilationUnit = {
      val driver =
        Java(scala.io.Source.fromInputStream(getClass.getResourceAsStream("OWASPDriver.java")).mkString).compilationUnit()
      val clazz = driver.getClassByName("OWASPDriver").get()
      val method = clazz.addMethod("testPost", Modifier.PUBLIC)
      val stmts = Seq(
        initCode,
        randomInit,
        cookieInit,
        cookieList,
        cookieSet,
        headers,
        header,
        param,
        paramMap,
        sendCode).flatten
      val body = new BlockStmt()
      stmts.foreach(s => body.addStatement(s))
      method.setBody(body)
      driver
    }

    val semanticType: Type =
      servletInit =>:
        randomInit =>:
        cookieInit =>:
        cookieMake =>:
        cookieSet =>:
        headersSet =>:
        headerSet =>:
        paramSet =>:
        paramsSet =>:
        callCode =>:
        owaspDriver
  }

  def forInhabitation: ReflectedRepository[Repository] = {
    ReflectedRepository(
      this,
      classLoader = this.getClass.getClassLoader
    )
  }

  def getResults(implicit resultLocation: ResultLocation): Results = {
    EmptyInhabitationBatchJobResults(this.forInhabitation)
      .addJob[CompilationUnit](owaspDriver)/*(persistable = new Persistable {
        type T = Seq[Statement]
        def rawText(elem: T): Array[Byte] = ???
        def path(elem: Seq[Statement]) = ???
      }, tag = implicitly)*/
      .compute()
  }
}
