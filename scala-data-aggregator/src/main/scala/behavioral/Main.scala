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
package behavioral
import behavioral.parse.Compare
import util.IO

object Main {
  val CommonPath = "/Users/lorand/Downloads/610/debug/raw-data/"
  val SmallOutputPath = IO.concatPath(CommonPath, "results-small.txt")
  val MediumOutputPath = IO.concatPath(CommonPath, "results-medium.txt")

  def main(args: Array[String]): Unit = {
    val smallClassFileName = "classes-2749-small.txt";
    val smallEffectsFileName = "effects-2748-small.txt";

    val mediumClassFileName = "classes-2750-medium.txt";
    val mediumEffectsFileName = "effects-2751-medium.txt";

    val classPath = IO.concatPath(CommonPath, smallClassFileName);
    val effectsPath = IO.concatPath(CommonPath, smallEffectsFileName);
//    val classPath = IO.concatPath(CommonPath, mediumClassFileName);
//    val effectsPath = IO.concatPath(CommonPath, mediumEffectsFileName);
    Compare.loadData(classPath, effectsPath)
    println("finished loading data")
    Compare.computeResults()
//    Compare.report(MediumOutputPath)
    Compare.report(SmallOutputPath)
    println("done.")
  }
}