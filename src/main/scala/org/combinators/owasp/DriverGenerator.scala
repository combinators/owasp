package org.combinators.owasp

import javax.inject.Inject
import org.combinators.cls.git.{InhabitationController, Results, RoutingEntries}
import org.combinators.cls.interpreter.CombinatorInfo
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

abstract class DriverController(webJars: WebJarsUtil, lifeCycle: ApplicationLifecycle)
  extends InhabitationController(webJars, lifeCycle)
    with RoutingEntries {
  val driverModel: OWASPDriverModel
  lazy val repository = new Repository(driverModel)
  lazy val Gamma = repository.forInhabitation
  override lazy val combinatorComponents: Map[String, CombinatorInfo] = Gamma.combinatorComponents
  override lazy val results: Results = repository.getResults
  override lazy val controllerAddress: String = "owasp"
}

class Benchmark13 @Inject()(webJars: WebJarsUtil, lifeCycle: ApplicationLifecycle)
  extends DriverController(webJars, lifeCycle) {
  lazy val driverModel: OWASPDriverModel =
    OWASPDriverModel(
      "BenchmarkTest00013",
      RandomCookie(Some(10)),
      StandardHeaderProvider(Some(RandomStringValue()), Some(Seq(RandomStringValue(), FixedStringValue("foo")))),
      StandardParameterProvider(Some(RandomStringValue()), Some((FixedStringValue("p"), Seq(RandomStringValue(), RandomStringValue())))),
      NoQuery
    )
}

