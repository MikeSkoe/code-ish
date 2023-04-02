// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  test("water is good") {
    given water: Root = Root.Water
    given good: Root = Root.Good

    assertNoDiff(
      showLex.show(
        water < good
      ),
      "water e good",
    )
  }

  test("a food has water") {
    given food: Root = Root.Food
    given have: Root = Root.Have
    given water: Root = Root.Water

    assertNoDiff(
      showLex.show(
        food ! have > water
      ),
      "food i have a water",
    )
  }

  test("my food strongly has good water") {
    given food: Root = Root.Food
    given me: Root = Root.Me
    given have: Root = Root.Have
    given strong: Root = Root.Strong
    given water: Root = Root.Water
    given good: Root = Root.Good

    assertNoDiff(
      showLex.show(
        food+me ! have+strong > water+good
      ),
      "food-me i have-strong a water-good",
    )
  }

  test("food has water and food has water") {
    given food: Root = Root.Food
    given me: Root = Root.Me
    given have: Root = Root.Have
    given strong: Root = Root.Strong
    given water: Root = Root.Water
    given good: Root = Root.Good

    assertNoDiff(
      showLex.show(
        food ! have > water `and` food ! have > water
      ),
      "food i have a water AND food i have a water",
    )
  }

  test("you and me are good") {
    given me: Root = Root.Me
    given you: Root = Root.You
    given good: Root = Root.Good

    assertNoDiff(
      showLex.show(
        me `and` you < good
      ),
      "me AND you e good",
    )
  }

  test("I have and make better a food") {
    given me: Root = Root.Me
    given have: Root = Root.Have
    given good: Root = Root.Good
    given food: Root = Root.Food

    assertNoDiff(
      showLex.show(
        me ! have `and` good > food
      ),
      "me i have AND good a food",
    )
  }
}

