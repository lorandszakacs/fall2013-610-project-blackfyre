package behavioral.parse

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import behavioral.parse._
import util.IO

@RunWith(classOf[JUnitRunner])
class DataTest extends FunSuite {
  val testDataLocation = "/Users/lorand/Downloads/610/debug/raw-data/test/"
  val classesFile = "classes-2749-small.txt"
  val effectsFile = "effects-2748-small.txt"

  val classFilesPath = IO.concatPath(testDataLocation, classesFile);
  val effectsFilePath = IO.concatPath(testDataLocation, effectsFile);

  test("isViolation = false;; two equal sets") {
    val m1 = new MethodInfo(null, "method1", List("f1", "f2", "f3", "f4"))
    val m2 = new MethodInfo(null, "method2", List("f1", "f2", "f3", "f4"))
    assert(m1.isViolation(m2) === false)
    assert(m2.isViolation(m1) === false)
  }
  
  test("isViolation = false;; two empty sets") {
    val m1 = new MethodInfo(null, "method1", List())
    val m2 = new MethodInfo(null, "method2", List())
    assert(m1.isViolation(m2) === false)
    assert(m2.isViolation(m1) === false)
  }
  
  test("isViolation = false;; same effects but in different order") {
    val m1 = new MethodInfo(null, "method1", List("f1", "f2", "f3"))
    val m2 = new MethodInfo(null, "method2", List("f1", "f3", "f2"))
    assert(m1.isViolation(m2) === false)
    assert(m2.isViolation(m1) === false)
  }
  
  test("isViolation = true") {
    val m1 = new MethodInfo(null, "method1", List("f1", "f2", "f3", "f4"))
    val m2 = new MethodInfo(null, "method2", List("f1", "f3", "f2"))
    assert(m1.isViolation(m2) === true)
    assert(m2.isViolation(m1) === true)
  }

  test("loadClasses") {
    Data.loadClasses(classFilesPath)
    val sts = Data.sts
    assert(sts.size === 4)

    val projects = Data.projects
    assert(projects.size === 3)
  }

  test("loadEffects") {
    Data.loadClasses(classFilesPath)
    Data.loadEffects(effectsFilePath)
    val sts = Data.sts
    assert(sts.size === 4)

    val projects = Data.projects
    assert(projects.size === 3)
    val data = Data.toString
    println(data)
  }

}