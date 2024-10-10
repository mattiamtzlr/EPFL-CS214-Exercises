package patmat

object TriBoolOps:
  import TriBool.*

  def neg(b: TriBool): TriBool = b match
    case Yes   => No
    case No    => Yes
    case Maybe => Maybe

  def allCases(
    yy: TriBool, mm: TriBool, nn: TriBool,
    yn: TriBool, ym: TriBool, nm: TriBool
  ) (b1: TriBool, b2: TriBool): TriBool = (b1, b2) match
    case (Yes, Yes) => yy
    case (Maybe, Maybe) => mm
    case (No, No) => nn

    case (Yes, No) | (No, Yes) => yn
    case (Yes, Maybe) | (Maybe, Yes) => ym
    case (No, Maybe) | (Maybe, No) => nm

  def and(b1: TriBool, b2: TriBool): TriBool =
    allCases(Yes, Maybe, No, No, Maybe, No)(b1, b2)

  def or(b1: TriBool, b2: TriBool): TriBool = 
    allCases(Yes, Maybe, No, Yes, Yes, Maybe)(b1, b2)

  def nand(b1: TriBool, b2: TriBool): TriBool =
    neg(and(b1, b2))
