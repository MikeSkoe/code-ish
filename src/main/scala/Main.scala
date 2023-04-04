enum Lex[Root, Marker, Conj]:
  case End()
  case Word(root: Root)
  case Conj(conj: Conj, lex: Lex[Root, Marker, Conj])
  case Phrase(marker: Marker, word: Word[Root, Marker, Conj], lex: Lex[Root, Marker, Conj])

trait Show[A]:
  def show(a: A): String

trait Parsable[A] extends Show[A]:
  val values: Array[String]
  def unshow(a: String): A

enum Root:
  case Mi, You, Love, Big, Not

enum Conj:
  case And, Or, Then

enum Marker:
  case Noun, Verb, Ad

object RootV extends Parsable[Root]:
  val values: Array[String] = Array(Root.Mi, Root.You, Root.Love, Root.Big, Root.Not).map(show)

  def unshow(a: String) =
    a match
      case "mi" => Root.Mi
      case "you" => Root.You
      case "love" => Root.Love
      case "big" => Root.Big
      case _ => Root.Not

  def show(a: Root): String =
    a match
      case Root.Mi => "mi"
      case Root.You => "you"
      case Root.Love => "love"
      case Root.Big => "big"
      case Root.Not => "not"

object ConjV extends Parsable[Conj]:
  val values = Array(Conj.And, Conj.Or, Conj.Then).map(show)

  def unshow(a: String) =
    a match
      case "and" => Conj.And
      case "or" => Conj.Or
      case _ => Conj.Then

  def show(a: Conj): String =
    a match
      case Conj.And => "and"
      case Conj.Or => "or"
      case Conj.Then => "then"

object MarkerV extends Parsable[Marker]:
  val values = Array(Marker.Noun, Marker.Verb, Marker.Ad).map(show)

  def unshow(a: String): Marker =
    a match
      case "a" => Marker.Noun 
      case "i" => Marker.Verb 
      case "e" => Marker.Ad

  def show(a: Marker): String =
    a match
      case Marker.Noun => "a"
      case Marker.Verb => "i"
      case Marker.Ad => "e"

def parse(
  input: List[String],
): Lex[Root, Marker, Conj] =
  input match
    case mark :: word :: tail if MarkerV.values.contains(mark) =>
      Lex.Phrase(MarkerV.unshow(mark), Lex.Word(RootV.unshow(word)), parse(tail))
    case mark :: tail if ConjV.values.contains(mark) =>
      Lex.Conj(ConjV.unshow(mark), parse(tail))
    case word :: tail => parse("e" :: word :: tail)
    case Nil => Lex.End()

@main def main(): Unit =
  println(parse("a mi mi i love love and i love a you you".split(" ").toList))
end main
