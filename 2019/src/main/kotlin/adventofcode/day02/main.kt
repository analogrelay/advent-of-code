package adventofcode.day02

import java.io.File

import adventofcode.common.IntcodeComputer

fun main(args: Array<String>) {
    var arguments = args.asList()
    val trace = arguments.contains("--trace")
    if (trace) {
        arguments = arguments.filterNot { it == "--trace" }
    }

    val fixError = arguments.contains("--fix-error")
    if (fixError) {
        arguments = arguments.filterNot { it == "--fix-error" }
    }

    val inputFile = if (arguments.size < 1) {
        System.err.println("Input file not selected. Choosing 'input.txt' in the current directory.")
        "input.txt"
    } else {
        arguments[0]
    }

    val program = File(inputFile).readText().split(",").map(::parseItem).toIntArray()
    solvePart1(program, trace, fixError)
    solvePart2(program, trace)
}

fun parseItem(input: String): Int = input.toInt()

fun solvePart1(program: IntArray, trace: Boolean, fixError: Boolean) {
    var memory = program.copyOf();
    if (fixError) {
        // Patch the 1202 error
        memory[1] = 12
        memory[2] = 2
    }

    val computer = IntcodeComputer(memory, trace)
    computer.runToHalt()

    println("[Part 1] Result: ${computer.memory[0]}")
}

const val TARGET = 19690720

fun solvePart2(program: IntArray, trace: Boolean) {
    for (noun in 0..99) {
        for (verb in 0..99) {
            var memory = program.copyOf()

            // Patch memory
            memory[1] = noun
            memory[2] = verb

            // Run and test the output
            val computer = IntcodeComputer(memory, trace)
            computer.runToHalt()

            if (computer.memory[0] == TARGET) {
                val result = (100 * noun) + verb
                println("[Part 2] Result: $result")
                return
            }
        }
    }

    println("[Part 2] PANIC: No result found")
}