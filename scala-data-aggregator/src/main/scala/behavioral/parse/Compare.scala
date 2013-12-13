/**
 * The MIT License (MIT)
 * 
 * Copyright (c) 2013 Lorand Szakacs
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package behavioral.parse
import scala.collection.mutable
import util.IO

class Violation(val project: Project, val child: ClassInfo, val parent: ClassInfo, val childMethod: MethodInfo, val parentMethod: MethodInfo) {

  override def toString: String = {
    "[ %s ][ %s   ===>  %s ][ %s ]".format(project.pid, child.fqn, parent.fqn, childMethod.name)
  }

}

object Compare {

  private var allViolations: mutable.ListBuffer[Violation] = mutable.ListBuffer.empty[Violation]
  private var allOverridenMethods: mutable.ListBuffer[(MethodInfo, MethodInfo)] = mutable.ListBuffer.empty[(MethodInfo, MethodInfo)]

  def loadData(classes: String, effects: String) {
    Data.loadClasses(classes);
    Data.loadEffects(effects);
  }

  private def compare() {
    val seenBefore: mutable.Set[String] = mutable.HashSet.empty
    for (pair <- Data.sts) {
      val childClass = Data.getClassForName(pair._1)
      val parentClass = Data.getClassForName(pair._2)
      val project = parentClass.project

      for (
        cM <- childClass.methods;
        pM <- parentClass.methods
      ) {
        if (cM.name == pM.name) {

          allOverridenMethods += ((cM, pM))
          if (cM.isViolation(pM)) {
            val violation = new Violation(project, childClass, parentClass, cM, pM)
            allViolations += violation
          }
        }
      } //end of combinatorial for
    }
  }

  var totalNumberOfProjects: Int = 0;
  var nrOfProjectsContainingViolations: Int = 0;

  var totalNumberOfSubtypingPairs: Int = 0;
  var totalNrOfClassLevelViolations: Int = 0;

  var totalNrOfOverridenMethods: Int = 0;
  var totalNrOfMethodLevelViolations: Int = 0;

  private var violationsByProject: Map[String, mutable.ListBuffer[Violation]] = Map.empty[String, mutable.ListBuffer[Violation]]
  private var nrOfMethodLevelViolationsPerProject: Map[String, Int] = Map.empty[String, Int]
  private var nrOfClassLevelViolationsPerProject: Map[String, Int] = Map.empty[String, Int]

  def computeResults() {
    compare();
    totalNumberOfProjects = Data.projects.size

    violationsByProject = allViolations.groupBy(v => v.project.pid)
    nrOfProjectsContainingViolations = violationsByProject.size

    totalNumberOfSubtypingPairs = Data.sts.size
    val violationsByPairsOfClasses = allViolations.groupBy(v => (v.child.fqn, v.parent.fqn))
    totalNrOfClassLevelViolations = violationsByPairsOfClasses.size

    totalNrOfOverridenMethods = allOverridenMethods.size
    totalNrOfMethodLevelViolations = allViolations.size

    nrOfMethodLevelViolationsPerProject = violationsByProject map (v => (v._1 -> v._2.size))
    val temp = violationsByProject map (v => (v._1 -> v._2.groupBy(f => (f.child.fqn, f.parent.fqn))))
    nrOfClassLevelViolationsPerProject = temp map (v => (v._1 -> v._2.size))
  }

  private lazy val generateReport: String = {
    var report = ""
    report += "total number of projects: " + totalNumberOfProjects + "\n"
    report += "total number of projects with violations: " + nrOfProjectsContainingViolations + "\n"
    report += "total number of class pairs: " + totalNumberOfSubtypingPairs + "\n"
    report += "total number of class level violations: " + totalNrOfClassLevelViolations + "\n"
    report += "total number of overriden methods:  " + totalNrOfOverridenMethods + "\n"
    report += "total number of method level violations: " + totalNrOfMethodLevelViolations + "\n"
    report += "Class level violations per project: " + "\n"
    report += nrOfClassLevelViolationsPerProject.map(p => "  %s -> %s".format(p._1, p._2)).mkString("\n") + "\n";
    report += "Method level violations per project: " + "\n"
    report += nrOfMethodLevelViolationsPerProject.map(p => "  %s -> %s".format(p._1, p._2)).mkString("\n") + "\n";

    report += "All violations: \n"
    val temp = violationsByProject map (pair => {
      val pid = pair._1
      val violations = pair._2
      val header = "%s\n".format(pid)
      val violationString = violations.map(v => "  %s".format(v.toString)).mkString("\n")
      header + violationString + "\n"
    })
    report += temp.mkString("\n")
    report += "\n===============\nProject Lengend\n===============\n"
    report += Data.projectLegend
    report
  }

  def report() {
    println(generateReport)
  }

  def report(filePath: String) {
    IO.writeToFile(generateReport.getBytes(), filePath);
  }

}