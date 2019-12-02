package com.github.anurse.adventofcode.day02

import java.io.File

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

    val computer = IntCodeComputer(memory, trace)
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
            val computer = IntCodeComputer(memory, trace)
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

class IntCodeComputer(public var memory: IntArray, private val trace: Boolean = false) {
    public var programCounter = 0

    // Run a single step of the program
    public fun step(): Boolean {
       val opcode = memory[programCounter]

       if (opcode == 99) {
           return false
       }

       val input1Addr = memory[programCounter + 1] 
       val input2Addr = memory[programCounter + 2] 
       val outputAddr = memory[programCounter + 3] 

       // Load inputs
       val input1 = memory[input1Addr]
       val input2 = memory[input2Addr]

       // Perform operation and store output
       memory[outputAddr] = execute(opcode, input1, input2)

       if (trace) {
           dumpState()
       }

       programCounter += 4
       return true
    }

    // Runs until the program halts
    public fun runToHalt() {
        while (step()) {}
    }

    private fun execute(opcode: Int, input1: Int, input2: Int): Int =
       if (opcode == 1) {
           input1 + input2
       } else if (opcode == 2) {
           input1 * input2
       } else {
           throw Exception("Unexpected opcode: $opcode");
       }

    private fun dumpState() {
        val dump = memory.map { it.toString() }.joinToString(",")
        println("State: $dump")
    }
}