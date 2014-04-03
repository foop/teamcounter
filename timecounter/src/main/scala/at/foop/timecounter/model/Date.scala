package at.foop.timecounter.model

class Date(val day: Integer, val month: Integer, val year: Integer) {
}

object Date {
  def apply(day: Integer, month: Integer, year: Integer): Date = {
    new Date(day, month, year)
  }
}

class Time(val hour: Integer, val minute: Integer) extends Ordered[Time] {

  require(hour >= 0, "the hour must be positive")
  require(hour <= 24, "the hour must be smaller than 24")
  require(minute >= 0, "the hour must be positive")
  require(minute <= 59, "the hour must be smaller than 59")

  def +(other: Time): Time = {
    Time(hour + other.hour, minute + other.minute)
  }
  def -(other: Time): Time = {
    Time(hour - other.hour, minute - other.minute)
  }

  def compare(that: Time) = {
    if (this.hour == that.hour) this.minute - that.minute
    this.hour - that.hour
  }

  override def equals(that: Any): Boolean = {
    if (that.isInstanceOf[Time]) {
      val otherTime = that.asInstanceOf[Time]
      otherTime.hour == this.hour && otherTime.minute == this.minute
    }
    false
  }

  override def hashCode(): Int = {
    (41 * (41 + hour) + minute)
  }

}

object Time {
  def apply(hour: Integer, minute: Integer): Time = {
    val newHour = hour % 24 + minute / 60
    new Time(newHour, minute % 60)
  }
}
