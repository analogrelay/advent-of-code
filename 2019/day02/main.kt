import java.io.File

fun main(args: Array<String>) {
    val inputFile = if (args.size < 1) {
        System.err.println("Input file not selected. Choosing 'input.txt' in the current directory.")
        "input.txt"
    } else {
        args[0]
    }

    val inputLines = File(inputFile).readLines()
    solve(inputLines)
}

fun solve(input: List<String>) {
    println("TODO: Solve!")
}