// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  test("Phrase to string") {
    assertNoDiff(
      Phrase(
        Lex.Subject(Word.Ad(Root.Mi, Word.Unit(Root.Manja))),
        Lex.Verb(Word.Unit(Root.Ave)),
        Lex.Object(Word.Ad(Root.Tu, Word.Unit(Root.Vasa))),
      ).toString(),
      "manja mi i ave a vasa tu",
    )

    assertNoDiff(
      Phrase(
        Lex.Subject(Word.Ad(Root.Ave, Word.Unit(Root.Vasa))),
        Lex.Adjective(Word.Unit(Root.Manja)),
      ).toString(),
      "vasa ave e manja",
    )
  }

  test("with conj") {
    assertNoDiff(
      Sentence(
        Phrase(
          Lex.Subject(Word.Con(
            Word.Unit(Root.Mi),
            Conj.An,
            Word.Unit(Root.Tu),
          )),
          Lex.Verb(Word.Unit(Root.Ave)),
          Lex.Object(Word.Unit(Root.Manja))
        ),
        Conj.Pero,
        Phrase(
          Lex.Subject(Word.Unit(Root.Si)),
          Lex.Adjective(Word.Unit(Root.Vasa))
        ),
      ).toString(),
      "mi AN tu i ave a manja PERO si e vasa"
    )
  }
}
