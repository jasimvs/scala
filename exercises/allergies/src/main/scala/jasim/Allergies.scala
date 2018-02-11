package jasim

object Allergies {

  def allergicTo(allergen: Allergen, score: Int): Boolean =
    (score & allergen.code) == allergen.code

  def list(score: Int): List[Allergen] =
    Allergen.allergens.filter(allegen => allergicTo(allegen, score))
}


case class Allergen(code: Int) {
  require(isPowerOf2(code))

  def isPowerOf2(number: Int): Boolean = {
    (code & (code - 1)) == 0
  }
}

object Allergen {

  val Eggs = Allergen(1)
  val Peanuts = Allergen(2)
  val Shellfish = Allergen(4)
  val Strawberries = Allergen(8)
  val Tomatoes = Allergen(16)
  val Chocolate = Allergen(32)
  val Pollen = Allergen(64)
  val Cats = Allergen(128)

  val allergens = List(Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats)
}