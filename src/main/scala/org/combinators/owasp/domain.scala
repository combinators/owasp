package org.combinators.owasp

case class OWASPDriverModel(
  benchmarkName: String,
  cookieProvider: CookieProvider,
  headerProvider: HeaderProvider,
  requestProvider: ParameterProvider,
  queryProvider: QueryProvider
) extends CanHaveRandomSource {

  val needsRandom: Boolean =
    Seq(
      cookieProvider,
      headerProvider,
      requestProvider,
      queryProvider
    ).exists(_.needsRandom)
}

trait CookieProvider extends CanHaveRandomSource
case object NoCookie extends CookieProvider with NoRandomSource
case class RandomCookie(fixedLength: Option[Int] = None) extends CookieProvider with HasRandomSource
case class FixedCookie(name: String, value: String) extends CookieProvider with NoRandomSource
case class MultipleCookies(cookies: Seq[CookieProvider]) extends CookieProvider with CanHaveRandomSource {
  val needsRandom: Boolean = cookies.exists(_.needsRandom)
}

trait HeaderProvider extends CanHaveRandomSource
trait StringValue extends CanHaveRandomSource
case class RandomStringValue(fixedLength: Option[Int] = None) extends StringValue with HasRandomSource
case class FixedStringValue(value: String) extends StringValue with NoRandomSource
case class StandardHeaderProvider(header: Option[StringValue], headers: Option[Seq[StringValue]]) extends HeaderProvider with CanHaveRandomSource {
  val needsRandom: Boolean = header.exists(_.needsRandom) || headers.exists(hs => hs.exists(_.needsRandom))
}

trait ParameterProvider extends CanHaveRandomSource

case class StandardParameterProvider(parameter: Option[StringValue], parameters: Option[(StringValue, Seq[StringValue])]) extends ParameterProvider with CanHaveRandomSource {
  val needsRandom: Boolean = parameter.exists(_.needsRandom) ||
    parameters.exists { case (k, v) => k.needsRandom || v.exists(_.needsRandom) }
}


trait ParameterNameProvider extends CanHaveRandomSource
case class FixedParameterName(name: String) extends ParameterNameProvider with NoRandomSource
case class RandomParameterName(fixedLength: Option[Int] = None) extends ParameterNameProvider with HasRandomSource

trait ParameterValueProvider extends CanHaveRandomSource
case class RandomParameterValue(fixedLength: Option[Int] = None) extends ParameterValueProvider with HasRandomSource
case object FixedParameterValue extends ParameterNameProvider with NoRandomSource
case class ParameterArray(contents: ParameterValueProvider*) extends ParameterValueProvider with CanHaveRandomSource {
  val needsRandom: Boolean = contents.exists(_.needsRandom)
}

trait QueryProvider extends CanHaveRandomSource
case class RandomQueryProvider(fixedLength: Option[Int] = None) extends QueryProvider with HasRandomSource
case object NoQuery extends QueryProvider with NoRandomSource

trait CanHaveRandomSource {
  val needsRandom: Boolean
}
trait NoRandomSource extends CanHaveRandomSource {
  val needsRandom: Boolean = false
}
trait HasRandomSource extends CanHaveRandomSource {
  val needsRandom: Boolean = true
}
