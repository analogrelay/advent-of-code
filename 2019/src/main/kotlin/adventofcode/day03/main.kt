package adventofcode.day03

import java.io.File

data class Point(val x: Int, val y: Int) {
    public fun moveLeft(distance: Int)= Point(x - distance, y)
    public fun moveRight(distance: Int)= Point(x + distance, y)
    public fun moveUp(distance: Int)= Point(x, y + distance)
    public fun moveDown(distance: Int)= Point(x, y - distance)

    public fun iterateLeft(distance: Int) = (1..distance).map { Point(x - it, y) }
    public fun iterateRight(distance: Int) = (1..distance).map { Point(x + it, y) }
    public fun iterateUp(distance: Int) = (1..distance).map { Point(x, y + it) }
    public fun iterateDown(distance: Int) = (1..distance).map { Point(x, y - it) }

    public fun manhattenDistance(p2: Point): Int =
        Math.abs(x - p2.x) + Math.abs(y - p2.y)
}

data class Line(val start: Point, val end: Point) {
    public fun hasPoint(p: Point): Boolean {
        val minX = Math.min(start.x, end.x)
        val maxX = Math.max(start.x, end.x)
        val minY = Math.min(start.y, end.y)
        val maxY = Math.max(start.y, end.y)

        return p.x >= minX && p.x <= maxX && p.y >= minY && p.y <= maxY
    }

    public fun intersects(other: Line): Point? {
        // Generate the intersection
        val intersection = if (end.y == start.y) {
            Point(other.start.x, start.y)
        } else {
            Point(start.x, other.start.y)
        }

        // Check if the intersection is on both lines
        if (other.hasPoint(intersection) && hasPoint(intersection)) {
            return intersection
        } else {
            return null
        }
    }
}

fun main(args: Array<String>) {
    val inputFile = if (args.size < 1) {
        System.err.println("Usage: adventofcode day03 <INPUT FILE>")
        System.exit(1)
        throw Exception("Whoop")
    } else {
        args[0]
    }

    val wires = File(inputFile).readLines().map(::parseItem)
    // println("[Part 1] Wire #1 ${wires[0]}")
    // println("[Part 1] Wire #2 ${wires[1]}")
    val intersections = (wires[0] intersect wires[1]).filterNot { it.x == 0 && it.y == 0 }
    val best = intersections.minBy { 
        println("[Part 1] Intersection at $it")
        it.manhattenDistance(Point(0, 0))
    }

    if (best == null) {
        println("[Part 1] No intersection found.")
    } else {
        val distance = best.manhattenDistance(Point(0, 0))
        println("[Part 1] Closest intersection is $best (distance: $distance)")
    }

    // Now compute the best intersection by distance along the line
    val bestBySteps = intersections.map {
        // Add two to include the end points
        Pair(it, wires[0].indexOf(it) + wires[1].indexOf(it) + 2)
    }.minBy { it.second }

    if (bestBySteps == null) {
        println("[Part 2] No intersection found.")
    } else {
        println("[Part 2] Closest intersection is ${bestBySteps.first} (distance: ${bestBySteps.second})")
    }
}

fun parseItem(line: String): List<Point> {
    var prev = Point(0, 0)
    return line.split(",").flatMap {
        val start = prev
        val distance = it.substring(1).toInt()
        when (it[0]) {
            'L' -> { 
                prev = start.moveLeft(distance)
                start.iterateLeft(distance)
            }
            'R' -> {
                prev = start.moveRight(distance)
                start.iterateRight(distance)
            }
            'D' -> {
                prev = start.moveDown(distance)
                start.iterateDown(distance)
            }
            'U' -> {
                prev = start.moveUp(distance)
                start.iterateUp(distance)
            }
            else -> throw Exception("Invalid direction: ${it[0]}")
        }
    }
}
