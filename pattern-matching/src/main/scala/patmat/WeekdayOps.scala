package patmat

import Weekday.*

object WeekdayOps:
  def next(d: Weekday): Weekday =
    values(scala.math.floorMod(d.ordinal + 1, values.length))

  def prev(d: Weekday): Weekday =
    values(scala.math.floorMod(d.ordinal - 1, values.length))