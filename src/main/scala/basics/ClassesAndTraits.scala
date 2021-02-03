package basics

object ClassesAndTraits {
  sealed trait Shape2D extends Located2D with Bounded2D with Movable2D{
    def area: Double
  }

  sealed trait Located2D {
    def x: Double
    def y: Double
  }

  sealed trait Bounded2D {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Movable2D {
    def move(dx: Double, dy: Double): Shape2D
  }

  final case class Point2D(x: Double, y: Double) extends Shape2D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def move(dx: Double, dy: Double): Shape2D = Point2D(x + dx, y + dy)
    override def area: Double = 0
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape2D {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def move(dx: Double, dy: Double): Shape2D = Circle(centerX + dx, centerY + dy, radius)
    override def area: Double = Math.PI * Math.pow(radius, 2)
  }

  final case class Rectangle(point1: Point2D, point2: Point2D) extends Shape2D {
    override def minX: Double = point1.x
    override def maxX: Double = point2.x
    override def minY: Double = point2.y
    override def maxY: Double = point1.y
    override def x: Double = point1.x / 2
    override def y: Double = point2.y / 2
    override def move(dx: Double, dy: Double): Shape2D = Rectangle(Point2D(point1.x + dx, point1.y + dy),
      Point2D(point2.x + dx, point2.y + dy))
    override def area: Double = ???
  }

  final case class Triangle(point1: Point2D, point2: Point2D, point3: Point2D) extends Shape2D {
    val bounded2D: Bounded2D=minimumBoundingRectangle(Set(point1, point2, point3))
    override def minX: Double = bounded2D.minX
    override def maxX: Double = bounded2D.maxX
    override def minY: Double = bounded2D.minY
    override def maxY: Double = bounded2D.maxY
    override def x: Double = ???
    override def y: Double = ???
    override def move(dx: Double, dy: Double): Shape2D = Triangle(Point2D(point1.x + dx, point1.y + dy),
      Point2D(point2.x + dx, point2.y + dy), Point2D(point3.x + dx, point3.y + dy))
    override def area: Double = (point1.x * (point2.y - point3.y) + point2.x * (point1.y - point3.y) +
      point3.x * (point1.y - point3.y)) / 2
  }

  final case class Square(rectangle: Rectangle) extends Shape2D {
    override def minX: Double = rectangle.minX
    override def maxX: Double = rectangle.maxX
    override def minY: Double = rectangle.minY
    override def maxY: Double = rectangle.maxY
    override def x: Double = rectangle.x
    override def y: Double = rectangle.y
    override def move(dx: Double, dy: Double): Shape2D = Square(Rectangle(Point2D(rectangle.point1.x + dx,
      rectangle.point1.y + dy), Point2D(rectangle.point2.x + dx, rectangle.point2.y + dy)))
    override def area: Double = ???
  }

  def minimumBoundingRectangle(objects: Set[Bounded2D]): Bounded2D = {
    new Bounded2D {
      implicit private val doubleOrdering: Ordering[Double] = Ordering.Double.IeeeOrdering
      override def minX: Double = objects.map(_.minX).min
      override def maxX: Double = objects.map(_.maxX).max
      override def minY: Double = objects.map(_.minY).min
      override def maxY: Double = objects.map(_.maxY).max
    }
  }

  sealed trait Located3D extends Located2D {
    def z: Double
  }

  sealed trait Bounded3D extends Bounded2D {
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Movable3D {
    def move(dx: Double, dy: Double, dz: Double): Shape3D
  }

  sealed trait Shape3D extends Located3D with Bounded3D with Movable3D {
    def volume: Double
    def surfaceArea: Double
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z
    override def move(dx: Double, dy: Double, dz: Double): Shape3D = Point3D(x + dx, y + dy, z + dz)
    override def volume: Double = 0
    override def surfaceArea: Double = 0
  }

  final case class Sphere(x: Double, y: Double, z: Double, radius: Double) extends Shape3D {
    override def volume: Double = 4 / 3 * Math.PI * Math.pow(radius, 3)
    override def surfaceArea: Double = 4 * Math.PI * Math.pow(radius, 2)
    override def move(dx: Double, dy: Double, dz: Double): Shape3D = Sphere(x+dx, y+dy, z+dz, radius)
    override def minZ: Double = z-radius
    override def maxZ: Double = z+radius
    override def minX: Double = x-radius
    override def maxX: Double = x+radius
    override def minY: Double = y-radius
    override def maxY: Double = y+radius
  }

  abstract class RightAnglesShape3D(point1: Point3D, point2: Point3D) extends Shape3D {
    val length: Double = point2.x - point1.x
    val width: Double = point1.y - point2.y
    val height: Double = point1.z - point2.z
    override def volume: Double = length * width * height
    override def surfaceArea: Double = 2 * (length * width + width * height + height * length)
    override def z: Double = (point2.z - point1.z) / 2 + point1.z
    override def x: Double = (point2.x - point1.x) / 2 + point1.x
    override def y: Double = (point1.y - point2.y) / 2 + point2.y
    override def minZ: Double = ???
    override def maxZ: Double = ???
    override def minX: Double = ???
    override def maxX: Double = ???
    override def minY: Double = ???
    override def maxY: Double = ???
    override def move(dx: Double, dy: Double, dz: Double): Shape3D = ???
  }

  final case class Cube(point1: Point3D, point2: Point3D) extends RightAnglesShape3D(point1, point2)

  final case class Cuboid(point1: Point3D, point2: Point3D) extends RightAnglesShape3D(point1, point2)

  final case class Triangle3D(point1:Point3D, point2:Point3D, point3:Point3D) extends Shape3D{
    override def volume: Double = ???
    override def surfaceArea: Double = ???
    override def minZ: Double = ???
    override def maxZ: Double = ???
    override def minX: Double = ???
    override def maxX: Double = ???
    override def minY: Double = ???
    override def maxY: Double = ???
    override def x: Double = ???
    override def y: Double = ???
    override def z: Double = ???
    override def move(dx: Double, dy: Double, dz: Double): Shape3D = ???
  }

  object Origin3D extends Located3D{
    override def x: Double = 0
    override def y: Double = 0
    override def z: Double = 0
  }

  object Origin extends Located2D {
    override def x: Double = 0
    override def y: Double = 0
  }

  // Homework
  //
  // Add additional 2D shapes such as triangle and square.
  //
  // In addition to the 2D shapes classes, add also 3D shapes classes
  // (origin, point, sphere, cube, cuboid, 3D triangle - you can add
  // others if you think they are a good fit).
  //
  // Add method `area` to 2D shapes.
  //
  // Add methods `surfaceArea` and `volume` to 3D shapes.
  //
  // If some of the implementation involves advanced math, it is OK
  // to skip it (leave unimplemented), the primary intent of this
  // exercise is modelling using case classes and traits, and not math.
}
