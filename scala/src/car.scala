/**
 * A generic car class that can hold any type of value.
 * @tparam T The type of value this car contains
 */
final case class Car[+T](value: T) {
  /**
   * Returns a string representation of the car.
   * @return String description of the car and its contents
   */
  override def toString: String = s"Car containing: $value"
}

/**
 * Companion object for Car class with factory methods
 */
object Car {
  /**
   * Creates a new Car with the given value
   * @param value The value to store in the car
   * @tparam T The type of the value
   * @return A new Car containing the value
   */
  def apply[T](value: T): Car[T] = new Car(value)
  
  /**
   * Safely extracts the value from a Car
   * @param car The car to extract the value from
   * @tparam T The type of the value in the car
   * @return The value contained in the car
   */
  def getValue[T](car: Car[T]): T = car.value
}