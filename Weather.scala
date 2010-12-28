/*
   Scala implementation of http://codekata.pragprog.com/2007/01/kata_four_data_.html (Part One)
	
   Copyright 2010 Tobias Bayer

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

import scala.util.matching.Regex

class Weather {

	def readFile(path: String) = {
		io.Source.fromFile(path).getLines
	}
	
	def isDataLine(line: String) = {
		val regExp = """^\d.*""".r
		regExp.findFirstIn(line.trim) != None
	}
	
	def maxTemp(line: String)  = {
		matchTemp(line, """^ *\d+ *(\d+).*""".r).toInt
	}
	
	def minTemp(line: String) = {
		matchTemp(line, """^ *\d+ *\d+ *(\d+).*""".r).toInt
	}
	
	def matchTemp(line: String, regExp: Regex) = {	
		line match {
			case regExp(temp) => temp
		}	
	}
	
	def diff(line: String) = {
		w.maxTemp(line) - w.minTemp(line)
	}
	
	def lowestDiffLine(path: String) = {
		val fileLines = readFile("weather.dat").filter(isDataLine)

		val ldl = fileLines.reduceLeft((a, b) => if(diff(a) > diff(b)) b else a) 
		ldl
	}
}

val w = new Weather()
println("Line with lowest temperature spread:")
println(w.lowestDiffLine("weather.dat"))
