enum Lex[Root, Marker, Conj]:
  case End()
  case Word(root: Root)
  case Conj(conj: Conj, lex: Lex[Root, Marker, Conj])
  case Phrase(marker: Marker, word: Word[Root, Marker, Conj], lex: Lex[Root, Marker, Conj])

trait Show[A]:
  def show(a: A): String

trait Parsable[A] extends Show[A]:
  def isCorrect(a: String): Boolean
  def unshow(a: String): A

enum Root:
  case End()
  case Mi(next: Root)
  case You(next: Root)
  case Love(next: Root)
  case Big(next: Root)
  case Not(next: Root)

enum Conj:
  case End, And, Or, Then

enum Marker:
  case End, Noun, Verb, Ad

object RootV extends Parsable[Root]:
  def isCorrect(a: String): Boolean = unshow(a) != Root.End()

  def unshow(a: String): Root =
    def iter(strings: List[String]): Root =
      strings match
        case "mi" :: next => Root.Mi(iter(next))
        case "you" :: next => Root.You(iter(next))
        case "love" :: next => Root.Love(iter(next))
        case "big" :: next => Root.Big(iter(next))
        case "not" :: next => Root.Not(iter(next))
        case _ => Root.End()
    iter(a.split("\\+").toList)

  def show(a: Root): String =
    def iter(r: Root): List[String] =
      r match
        case Root.End() => Nil
        case Root.Mi(next) => "mi" :: iter(next)
        case Root.You(next) => "you" :: iter(next)
        case Root.Love(next) => "love" :: iter(next)
        case Root.Big(next) => "big" :: iter(next)
        case Root.Not(next) => "not" :: iter(next)
    iter(a).reduce(_ + "+" + _)

object ConjV extends Parsable[Conj]:
  def isCorrect(a: String): Boolean = unshow(a) != Conj.End
  def unshow(a: String) =
    a match
      case "and" => Conj.And
      case "or" => Conj.Or
      case "then" => Conj.Then
      case _ =>  Conj.End

  def show(a: Conj): String =
    a match
      case Conj.And => "and"
      case Conj.Or => "or"
      case Conj.Then => "then"
      case Conj.End => ""

object MarkerV extends Parsable[Marker]:
  def isCorrect(a: String): Boolean = unshow(a) != Marker.End
  def unshow(a: String): Marker =
    a match
      case "a" => Marker.Noun 
      case "i" => Marker.Verb 
      case "e" => Marker.Ad
      case _ => Marker.End

  def show(a: Marker): String =
    a match
      case Marker.End => ""
      case Marker.Noun => "a"
      case Marker.Verb => "i"
      case Marker.Ad => "e"

object LexV extends Parsable[Lex[Root, Marker, Conj]]:
  def isCorrect(a: String): Boolean = unshow(a) != Lex.End()

  def unshow(input: String): Lex[Root, Marker, Conj] =
    input.split(" ").toList match
      case mark :: tail if MarkerV.isCorrect(mark) => iter(mark :: tail)
      case tail => iter(MarkerV.show(Marker.Noun) :: tail)

  private def iter(input: List[String]): Lex[Root, Marker, Conj] =
    input match
      case mark :: word :: tail if MarkerV.isCorrect(mark) =>
        Lex.Phrase(MarkerV.unshow(mark), Lex.Word(RootV.unshow(word)), iter(tail))
      case mark :: tail if ConjV.isCorrect(mark) =>
        Lex.Conj(ConjV.unshow(mark), iter(tail))
      case word :: tail => iter(MarkerV.show(Marker.Ad) :: word :: tail)
      case Nil => Lex.End()

  def show(lex: Lex[Root, Marker, Conj]): String =
    lex match
      case Lex.End() => ""
      case Lex.Word(root) => RootV.show(root)
      case Lex.Conj(conj, lex) => s"${ConjV.show(conj)} ${show(lex)}"
      case Lex.Phrase(marker, word, lex) => s"${MarkerV.show(marker)} ${show(word)} ${show(lex)}"

@main def main(): Unit =
  val input = "mi+you mi i love big a you"
  val parsed = LexV.unshow(input)
  val showd = LexV.show(parsed)

  println(input)
  println(showd)
end main
