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

class Project(val pid: String, val name: String, val url: String) extends Equals {
  private var classesMutable: mutable.Map[String, ClassInfo] = mutable.HashMap.empty[String, ClassInfo]

  def addClassIfNew(c: ClassInfo) {
    if (!classesMutable.contains(c.fqn)) {
      classesMutable += (c.fqn -> c)
    }
    c.addProject(this);
  }

  def classes: Set[ClassInfo] = classesMutable.values.toSet

  def contains(c: ClassInfo) = classesMutable.contains(c.fqn)

  def contains(s: String) = classesMutable.contains(s)

  def get(s: String) = classesMutable.get(s)

  def canEqual(other: Any) = {
    other.isInstanceOf[behavioral.parse.Project]
  }

  override def equals(other: Any) = {
    other match {
      case that: behavioral.parse.Project => that.canEqual(Project.this) && pid == that.pid
      case _ => false
    }
  }

  override def hashCode() = {
    val prime = 41
    prime + pid.hashCode
  }

  override def toString: String = "%s\n".format(pid) + classesMutable.values.mkString("\n    ")

}

class ClassInfo(val pid: String, val fqn: String) extends Equals {
  private var methMutable: mutable.ListBuffer[MethodInfo] = mutable.ListBuffer.empty[MethodInfo];
  private var proj: Project = null;

  def addMethod(m: MethodInfo) {
    methMutable += m;
  }

  def addProject(p: Project) {
    proj = p;
  }

  def project: Project = {
    if (this.proj == null)
      throw new RuntimeException("Project reference is null in %s".format(fqn))
    this.proj
  }

  def methods: List[MethodInfo] = methMutable.toList

  def canEqual(other: Any) = {
    other.isInstanceOf[behavioral.parse.ClassInfo]
  }

  override def equals(other: Any) = {
    other match {
      case that: behavioral.parse.ClassInfo => that.canEqual(ClassInfo.this) && fqn == that.fqn && pid == that.pid
      case _ => false
    }
  }

  override def hashCode() = {
    val prime = 41
    prime + fqn.hashCode + pid.hashCode
  }

  override def toString: String = "[%s][%s]".format(pid, fqn) + methMutable.mkString("\n        ")
}

class MethodInfo(val owner: ClassInfo, val name: String, val effects: List[String]) extends Equals {

  def isViolation(that: MethodInfo): Boolean = {
    val tempOriginal = effects.toSet
    val tempThat = that.effects.toSet
    (!tempOriginal.equals(tempThat))
  }

  def canEqual(other: Any) = {
    other.isInstanceOf[behavioral.parse.MethodInfo]
  }

  override def equals(other: Any) = {
    other match {
      case that: behavioral.parse.MethodInfo => that.canEqual(MethodInfo.this) && owner == that.owner && name == that.name
      case _ => false
    }
  }

  override def hashCode() = {
    val prime = 41
    prime * (prime + owner.hashCode) + name.hashCode
  }

  override def toString: String = "%s.%s=%s".format(owner.fqn, name, effects mkString (" "))
}

object Data {
  private var projectsMutable: mutable.Map[String, Project] = mutable.HashMap.empty[String, Project]
  private var stsMutable: mutable.Map[String, String] = mutable.HashMap.empty[String, String]
  private var classes: mutable.Map[String, ClassInfo] = mutable.HashMap.empty[String, ClassInfo]

  def projects: Map[String, Project] = projectsMutable.toMap

  def sts: Map[String, String] = stsMutable.toMap

  def loadClasses(filePath: String) {
    val lines = IO.readLines(filePath)
    val partition = lines.partition(_.startsWith(Parser.ClassLineStart))
    val clsRaw = partition._1
    val stsRaw = partition._2

    val clsLines = clsRaw map (l => new ClassLine(l))
    val stsLines = stsRaw map (l => new StsLine(l))

    for (sts <- stsLines) {
      val childClass = new ClassInfo(sts.pid, sts.childFQN)
      val parentClass = new ClassInfo(sts.pid, sts.parentFQN)
      Data.addMapping(childClass, parentClass)
    }

    for (cls <- clsLines) {
      if (Data.careAbout(cls.className)) {
        //if we care about the class then it will always be there.
        val careAbout = Data.getClassForName(cls.className)
        if (Data.projectsMutable.contains(cls.pid)) {
          val proj = Data.projectsMutable.get(cls.pid).get
          proj.addClassIfNew(careAbout)
          Data.projectsMutable += (proj.pid -> proj)
        } else {
          val proj = new Project(cls.pid, cls.projectName, cls.projectURL)
          proj.addClassIfNew(careAbout)
          Data.projectsMutable += (proj.pid -> proj);
        }
      } //end if care about.
    }
  }

  def getClassForName(fqn: String): ClassInfo = {
    classes.get(fqn).get
  }

  def loadEffects(filePath: String) {
    def validLine(effLine: EffectLine): Boolean = {
      !effLine.effects.contains("<abstract>") && !effLine.methodName.contains("<init>") && !effLine.methodName.contains("<clinit>")
    }
    val lines = IO.readLines(filePath);
    for (line <- lines) {
      val effLine = new EffectLine(line)
      if (validLine(effLine) && projectsMutable.contains(effLine.pid)) {
        val proj = projectsMutable.get(effLine.pid).get
        if (proj.contains(effLine.fqn)) {
          val clazz = proj.get(effLine.fqn).get
          val meth = new MethodInfo(clazz, effLine.methodName, effLine.effects)
          clazz.addMethod(meth)
        }
      }
    }
  }

  lazy val projectLegend: String = {
    val temp = projectsMutable map (p => {
      val proj = p._2;
      "[ %s ][ %s ][ %s ]".format(proj.pid, proj.name, proj.url)
    })
    temp.mkString("\n")
  }

  override def toString: String = projectsMutable.values.mkString("\n")

  //if it was not present in the subtype->supertype mapping we don't care about it
  private def careAbout(c: String): Boolean = return classes.contains(c)

  private def addMapping(child: ClassInfo, parent: ClassInfo) {
    if (!stsMutable.contains(child.fqn)) {
      stsMutable += (child.fqn -> parent.fqn)
      if (!classes.contains(child.fqn)) {
        classes += (child.fqn -> child)
      }
      if (!classes.contains(parent.fqn)) {
        classes += (parent.fqn -> parent)

      }
    }
  }
}