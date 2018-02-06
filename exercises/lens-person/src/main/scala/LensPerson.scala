import java.time.LocalDate

import monocle.Lens

object LensPerson {
  case class Person(_name: Name, _born: Born, _address: Address)

  case class Name(_foreNames: String /*Space separated*/ , _surName: String)

  // Value of java.time.LocalDate.toEpochDay
  type EpochDay = Long

  case class Born(_bornAt: Address, _bornOn: EpochDay)

  case class Address(_street: String, _houseNumber: Int,
    _place: String /*Village / city*/ , _country: String)

  // Valid values of Gregorian are those for which 'java.time.LocalDate.of'
  // returns a valid LocalDate.
  case class Gregorian(_year: Int, _month: Int, _dayOfMonth: Int)

  // Implement these.

  import monocle.macros.GenLens

  val personLens = GenLens[Person]
  val name: Lens[Person, Name] = personLens(_._name)
  val born: Lens[Person, Born] = personLens(_._born)
  val address: Lens[Person, Address] = personLens(_._address)

  val nameLens = GenLens[Name]
  val foreNames: Lens[Name, String] = nameLens(_._foreNames)
  val surName: Lens[Name, String] = nameLens(_._surName)

  val bornLens = GenLens[Born]
  val bornAt: Lens[Born, Address] = bornLens(_._bornAt)
  val bornOn: Lens[Born, EpochDay] = bornLens(_._bornOn)

  val street: Lens[Address, String] = GenLens[Address](_._street)

  val bornStreet: Born => String =
    born => bornAt composeLens street get(born)

  val setCurrentStreet: String => Person => Person =
    newStreet => address composeLens street modify(_ => newStreet)

  val setBirthMonth: Int => Person => Person =
    month =>  born composeLens bornOn modify(epoch =>
      LocalDate.ofEpochDay(epoch).withMonth(month).toEpochDay)

  // Transform both birth and current street names.
  val renameStreets: (String => String) => Person => Person =
    renameFn => {
      val step1 = born composeLens bornAt composeLens street modify(renameFn)
      val step2 = address composeLens street modify(renameFn)
      step1 compose step2
    }

}
