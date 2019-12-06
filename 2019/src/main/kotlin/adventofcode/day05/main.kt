package adventofcode.day05

fun main(args: Array<String>) {
    val inputFile = if (args.size < 1) {
        System.err.println("Usage: adventofcode day05 <INPUT FILE>")
        System.exit(1)
        throw Exception("Whoop")
    } else {
        args[0]
    }
    println("Using input file $inputFile");
}