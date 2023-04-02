import scala.collection.immutable.List

enum Conj:
  case And, But, Or, In, On, Of

enum Root:
  case Me, You, It, Food, Have, Water, Strong, Good

enum Word:
  case Unit(root: Root)
  case Comb(root: Root, word: Word)

enum Lex:
  case Unit(word: Word) // Subject
  case Verb(word: Word, lex: Lex)
  case Object(word: Word, lex: Lex)
  case Adjective(word: Word, lex: Lex)
  case Conjaction(left: Lex, conj: Conj, right: Lex)

// Utils
//  TODO: remove a lot of boilerplate

// + (word combination)
extension (a: Root)
  def +(b: Root): Word = Word.Comb(a, Word.Unit(b))

// ! (verb)
extension (a: Root)
  def !(b: Root): Lex = Lex.Verb(Word.Unit(a), Lex.Unit(Word.Unit(b)))
  def !(b: Lex): Lex = Lex.Verb(Word.Unit(a), b)
extension (a: Word)
  def !(b: Root): Lex = Lex.Verb(a, Lex.Unit(Word.Unit(b)))
  def !(b: Lex): Lex = Lex.Verb(a, b)

// > (direct object)
extension (a: Root)
  def >(b: Root): Lex = Lex.Object(Word.Unit(a), Lex.Unit(Word.Unit(b)))
extension (a: Word)
  def >(b: Root): Lex = Lex.Object(a, Lex.Unit(Word.Unit(b)))
  def >(b: Word): Lex = Lex.Object(a, Lex.Unit(b))

// < (adjective)
extension (a: Root)
  def <(b: Root): Lex = Lex.Adjective(Word.Unit(a), Lex.Unit(Word.Unit(b)))
extension (a: Word)
  def <(b: Root): Lex = Lex.Adjective(a, Lex.Unit(Word.Unit(b)))
  def <(b: Word): Lex = Lex.Adjective(a, Lex.Unit(b))

// Conjunction functions
extension (left: Lex)
  infix def and(right: Lex): Lex = Lex.Conjaction(left, Conj.And, right)
  infix def or(right: Lex): Lex = Lex.Conjaction(left, Conj.Or, right)
  infix def but(right: Lex): Lex = Lex.Conjaction(left, Conj.But, right)
  infix def in(right: Lex): Lex = Lex.Conjaction(left, Conj.In, right)
  infix def on(right: Lex): Lex = Lex.Conjaction(left, Conj.On, right)
  infix def of(right: Lex): Lex = Lex.Conjaction(left, Conj.Of, right)

extension (left: Root)
  infix def and(right: Lex): Lex = Lex.Conjaction(Lex.Unit(Word.Unit(left)), Conj.And, right)
  infix def or(right: Lex): Lex = Lex.Conjaction(Lex.Unit(Word.Unit(left)), Conj.Or, right)
  infix def but(right: Lex): Lex = Lex.Conjaction(Lex.Unit(Word.Unit(left)), Conj.But, right)
  infix def in(right: Lex): Lex = Lex.Conjaction(Lex.Unit(Word.Unit(left)), Conj.In, right)
  infix def of(right: Lex): Lex = Lex.Conjaction(Lex.Unit(Word.Unit(left)), Conj.Of, right)

// Render

trait Show[A]:
  def show(a: A): String

given conjShow: Show[Conj] with
  def show(c: Conj): String = c match
    case Conj.And => "AND"
    case Conj.But => "BUT"
    case Conj.Or => "OR"
    case Conj.In => "IN"
    case Conj.On => "ON"
    case Conj.Of => "OF"

given rootShow: Show[Root] with
  def show(r: Root): String = r match
    case Root.Me => "me"
    case Root.You => "you"
    case Root.It => "it"
    case Root.Food => "food"
    case Root.Have => "have"
    case Root.Water => "water"
    case Root.Strong => "strong"
    case Root.Good => "good"

given showWord(using showRoot: Show[Root]): Show[Word] with
  def show(w: Word): String = w match
    case Word.Unit(root) => showRoot.show(root)
    case Word.Comb(root, word) => s"${showRoot.show(root)}-${show(word)}"

given showLex(using showWord: Show[Word], showConj: Show[Conj]): Show[Lex] with
  val subject = "?"
  val verbMark = "i"
  val objectMark = "a"
  val adjectiveMark = "e"

  def show(l: Lex): String = l match
    case Lex.Unit(word) => showWord.show(word)
    case Lex.Verb(word, lex) => s"${showWord.show(word)} $verbMark ${show(lex)}"
    case Lex.Object(word, lex) => s"${showWord.show(word)} $objectMark ${show(lex)}"
    case Lex.Adjective(word, lex) => s"${showWord.show(word)} $adjectiveMark ${show(lex)}"
    case Lex.Conjaction(left, conj, right) => s"${show(left)} ${showConj.show(conj)} ${show(right)}"

@main def main(): Unit =
  given food: Root = Root.Food
  given me: Root = Root.Me
  given have: Root = Root.Have
  given it: Root = Root.It
  given water: Root = Root.Water

  println(
    showLex.show(
      food ! have > water
    )
  )
end main

