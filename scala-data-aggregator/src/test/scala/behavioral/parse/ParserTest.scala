package behavioral.parse

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import behavioral.parse._
import util.IO

@RunWith(classOf[JUnitRunner])
class ParserTest extends FunSuite {

  test("line with effects") {
    val data = "effects[102990][com.j2xtreme.xapp.auth.ProtectedApplication][setUnauthorizedURL] =  unauthorizedURL"
    val result = new EffectLine(data)
    assert(result.pid === "102990")
    assert(result.fqn === "com.j2xtreme.xapp.auth.ProtectedApplication")
    assert(result.methodName === "setUnauthorizedURL")
    assert(result.effects.length === 1)
    assert(result.effects(0) === "unauthorizedURL")
  }

  test("line with no efects") {
    val data = "effects[102990][com.j2xtreme.xapp.auth.ProtectedApplication][setUnauthorizedURL] = "
    val result = new EffectLine(data)
    assert(result.pid === "102990")
    assert(result.fqn === "com.j2xtreme.xapp.auth.ProtectedApplication")
    assert(result.methodName === "setUnauthorizedURL")
    assert(result.effects.isEmpty === true)
  }

  test("ClassLine") {
    val data = "cls[org.apache.poi.util.POILogger] = 109474###Gadzoink###http://sourceforge.net/projects/gadzoink"
    val result = new ClassLine(data)
    assert(result.className === "org.apache.poi.util.POILogger")
    assert(result.pid === "109474")
    assert(result.projectName === "Gadzoink")
    assert(result.projectURL === "http://sourceforge.net/projects/gadzoink")
  }

  test("StsLine") {
    val data = "sts[242839][org.antlr.runtime.Parser] = org.antlr.runtime.BaseRecognizer"
    val result = new StsLine(data);
    assert(result.pid === "242839")
    assert(result.childFQN === "org.antlr.runtime.Parser")
    assert(result.parentFQN === "org.antlr.runtime.BaseRecognizer")
  }
}