
import scala.io.StdIn
import scala.util.parsing.combinator.RegexParsers

object Main {

  case class Command(val command: String,
                     val id: Option[Int],
                     val value: Option[String])

  object VHParser extends RegexParsers {

    def keyword: Parser[String] =
      "put" | "get" | "delete"

    def idKey: Parser[Int] =
      "(0|[1-9]\\d*)".r ^^ (a => a.toInt)

    def valueKey: Parser[String] =
      ".*".r

    def command: Parser[Command] =
      keyword ~ opt(idKey) ~ opt(":" ~ valueKey) ^^ {
        case kw ~ id ~ Some(_ ~ value) => Command(kw, id, Option.apply(value))
        case kw ~ id ~ None => Command(kw, id, Option.empty)
      }
  }

  def parse(str: String): Command = VHParser.parse(VHParser.command, str) match {
    case _: VHParser.Failure => throw new IllegalStateException("Parse error")
    case result => result.get
  }

  def evaluateCommand(cmd: Command)(in: Map[Int, String]): Map[Int,String] = cmd match {
    case Command("put", id, Some(value)) => id match {
      case None => throw new IllegalStateException("No id provided")
      case Some(id) => in.+((id, value))
    }
    case Command("put", _, None) => throw new IllegalStateException("No content provided")
    case Command("get", None, _) =>
      println("All entries: ")
      in.map(x => x._1 + ": " + x._2).foreach(println(_))
      in
    case Command("get", Some(id), _) => in.get(id) match {
      case None =>
        println("ID " + id + " not found")
      case Some(value) =>
        println("== " + id + " ==")
        println(value)
    }; in
    case Command("delete", None, _) =>
      println("Deleted all entries")
      Map()
    case Command("delete", Some(id), _) => in.find(x => x._1 == id) match {
      case None => println("ID " + id + " does not exists"); in
      case Some(_) => in.takeWhile(x => x._1 == id)
    }
    case Command(command, _, _) => throw new IllegalStateException("Invalid command " + command)
  }

  var videos: Map[Int, String] = Map[Int, String]()

  def main(args: Array[String]): Unit = {
    while (true) try {
      videos = evaluateCommand(parse(StdIn.readLine("> ")))(videos)
    } catch {
      case e: IllegalStateException => println(e.getLocalizedMessage)
      case _: Exception =>
        System.exit(1)
    }
  }

}