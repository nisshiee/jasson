package org.onion_lang.jasson
import io.Source
import util.parsing.json.JSON
import java.io.{Reader, FileReader}

object JassonTool {
  def using[T <: { def close(): Unit }, V](resource: T)(block: T => V): V = try {
    block(resource)
  } finally {
    resource.close()
  }
  def readAll(reader: Reader): String = {
    Iterator.continually(reader.read()).takeWhile(_ != -1).map(_.toChar).mkString("")
  }
  def main(args: Array[String]) {
    args match {
      case Array(inputFile) =>
        using(new FileReader(inputFile)){r =>
          val input = readAll(r)
          val schema = JaconParser.parseFrom(input) match {
            case JaconParser.Success(value, _) =>
              value
            case JaconParser.Failure(msg, _) =>
              Console.err.println(msg)
              return
            case JaconParser.Error(msg, _) =>
              Console.err.println(msg)
              return
          }
          CodeGenerator(".").generateCode(schema)
        }
      case _ =>
        println("Usage: scala JassonTool -d <dir_name> input.json")
    }
  }
}
