import scala.collection.immutable.List

enum Conj:
  case And, But, Or, Then

enum Root:
  case Me, You, It, Food, Have, Water

enum Word:
  case Unit(root: Root)
  case Comb(word: Word, root: Root)

enum Lex:
  case Unit(word: Word) // Subject
  case Verb(lex: Lex, word: Word)
  case Object(lex: Lex, word: Word)
  case Adjective(lex: Lex, word: Word)

// Utils

// + (word comb)
extension (a: Word)
  def +(b: Root): Word = Word.Comb(a, b)
extension (a: Root)
  def +(b: Root): Word = Word.Comb(Word.Unit(a), b)

// ! (verb)
extension (a: Lex)
  def !(b: Word): Lex = Lex.Verb(a, b)
extension (a: Word)
  def !(b: Word): Lex = Lex.Verb(Lex.Unit(a), b)
extension (a: Word)
  def !(b: Root): Lex = Lex.Verb(Lex.Unit(a), Word.Unit(b))

// > (direct object)
extension (a: Lex)
  def >(b: Word): Lex = Lex.Object(a, b)
extension (a: Lex)
  def >(b: Root): Lex = Lex.Object(a, Word.Unit(b))
extension (a: Word)
  def >(b: Word): Lex = Lex.Object(Lex.Unit(a), b)
extension (a: Word)
  def >(b: Root): Lex = Lex.Object(Lex.Unit(a), Word.Unit(b))

// Render

trait Show[A]:
  def show(a: A): String

given conjShow: Show[Conj] with
  def show(c: Conj): String = c match
    case Conj.And => "AND"
    case Conj.But => "BUT"
    case Conj.Or => "OR"
    case Conj.Then => "THEN"

given rootShow: Show[Root] with
  def show(r: Root): String = r match
    case Root.Me => "me"
    case Root.You => "you"
    case Root.It => "it"
    case Root.Food => "food"
    case Root.Have => "have"
    case Root.Water => "water"

given showWord(using showRoot: Show[Root]): Show[Word] with
  def show(w: Word): String = w match
    case Word.Unit(root) => showRoot.show(root)
    case Word.Comb(word, root) => s"${show(word)}-${showRoot.show(root)}"

given showLex(using showWord: Show[Word]): Show[Lex] with
  val verbMark = "i"
  val objectMark = "a"
  val adjectiveMark = "e"

  def show(l: Lex): String = l match
    case Lex.Unit(word) => showWord.show(word)
    case Lex.Verb(lex, word) => s"${show(lex)} $verbMark ${showWord.show(word)}"
    case Lex.Object(lex, word) => s"${show(lex)} $objectMark ${showWord.show(word)}"
    case Lex.Adjective(lex, word) => s"${show(lex)} $adjectiveMark ${showWord.show(word)}"

@main def main(): Unit =
  given food: Root = Root.Food
  given me: Root = Root.Me
  given have: Root = Root.Have
  given it: Root = Root.It

  println(
    // showLex.show(
      (food + me) ! (have + it) > it
    // )
  )
end main

