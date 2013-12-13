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

object Parser {
  def consumeBracket(line: String): (String, String) = {
    val firstOpenBracket = line.indexOf('[');
    val first = line.substring(firstOpenBracket + 1);
    val firstClosedBracket = first.indexOf(']');
    val content = first.substring(0, firstClosedBracket);
    val remainder = first.substring(firstClosedBracket + 1);
    (content, remainder);
  }

  final val ClassLineStart = "cls"
  final val EffectLineStart = "effects"
  final val StsLineStart = "sts"
}

class EffectLine(line: String) {
  require(line.startsWith(Parser.EffectLineStart));
  EffectLine.construct(line);
  val pid: String = EffectLine.values._1
  val fqn: String = EffectLine.values._2
  val methodName: String = EffectLine.values._3
  val effects: List[String] = EffectLine.values._4

  override def toString(): String =
    pid + " " + fqn + " " + methodName + " = " + effects.mkString(" ")

}
private object EffectLine {
  private var values = ("", "", "", List(""));
  private def construct(line: String) = {
    val firstBracket = Parser.consumeBracket(line)
    val pid = firstBracket._1

    val secondBracket = Parser.consumeBracket(firstBracket._2)
    val fqn = secondBracket._1

    val thirdBracket = Parser.consumeBracket(secondBracket._2)
    val methodName = thirdBracket._1

    val effectsString = line.split("=")(1)
    val effects = effectsString.split(" ").filterNot(_.equals("")).toList

    values = (pid, fqn, methodName, effects)
  }

}

class ClassLine(line: String) {
  require(line.startsWith(Parser.ClassLineStart))
  ClassLine.construct(line);
  val className = ClassLine.values._1;
  val pid = ClassLine.values._2;
  val projectName = ClassLine.values._3;
  val projectURL = ClassLine.values._4;

  override def toString(): String =
    className + " " + pid + " " + projectName + " = " + projectURL
}

private object ClassLine {
  private var values = ("", "", "", "");
  private def construct(line: String) = {
    //cls[org.apache.poi.util.POILogger] = 109474###Gadzoink###http://sourceforge.net/projects/gadzoink
    val className = Parser.consumeBracket(line)._1

    val projectData = line.split(" = ")(1).split("###");
    values = (className, projectData(0), projectData(1), projectData(2));
  }

}

class StsLine(line: String) {
  require(line.startsWith(Parser.StsLineStart))
  StsLine.construct(line)
  val pid = StsLine.values._1;
  val childFQN = StsLine.values._2;
  val parentFQN = StsLine.values._3;
}

private object StsLine {
  private var values = ("", "", "");
  private def construct(line: String) = {
    //sts[p.id][com.drew.imaging.jpeg.JpegProcessingException] = com.drew.lang.CompoundException
    val first = Parser.consumeBracket(line)
    val pid = first._1
    val childFQN = Parser.consumeBracket(first._2)._1

    val parentFQN = line.split(" = ")(1)
    values = (pid, childFQN, parentFQN);
  }
}