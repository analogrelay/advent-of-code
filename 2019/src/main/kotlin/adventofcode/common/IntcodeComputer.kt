package adventofcode.common

import java.io.File

enum class ParameterMode {
    Position,
    Immediate;

    companion object {
        public fun decode(modeVal: Int) = ParameterMode.values()[modeVal]
    }
}

data class Instruction(public val opcode: Int, public val modes: List<ParameterMode>) {
    companion object {
        fun decode(encoded: Int): Instruction {
            // Get the opcode, which is the lower two digits
            val opcode = encoded % 100
            var modeFlags = encoded / 100

            val modes = generateSequence {
                if (modeFlags == 0) {
                    null
                } else {
                    val mode = ParameterMode.decode(modeFlags % 10)
                    modeFlags = modeFlags / 10
                    mode
                }
            }.toList()

            return Instruction(opcode, modes)
        }
    }

    public fun modeFor(parameterIndex: Int) =
        if (parameterIndex >= modes.size) {
            ParameterMode.Position
        } else {
            modes[parameterIndex]
        }
}

public class IntcodeComputer(public var memory: IntArray, private val trace: Boolean = false) {
    public var programCounter = 0

    companion object {
        public fun fromProgram(input: String, trace: Boolean = false): IntcodeComputer {
            val memory = input.split(",").map { it.toInt() }.toIntArray()
            return IntcodeComputer(memory, trace)
        }

        public fun fromProgramFile(inputFile: String, trace: Boolean = false): IntcodeComputer =
            fromProgram(File(inputFile).readText().trim(), trace)
    }

    private fun readNext(): Int {
        val value = memory[programCounter]
        programCounter += 1
        return value
    }

    private fun readNextByMode(mode: ParameterMode): Int {
        val value = readNext()

        return when (mode) {
            ParameterMode.Position -> memory[value]
            ParameterMode.Immediate -> value
        }
    }

    // Run a single step of the program
    public fun step(): Boolean {
        var startPc = programCounter
        val instruction = Instruction.decode(readNext())

        fun execBinop(op: (Int, Int) -> Int) {
            val x = readNextByMode(instruction.modeFor(0))
            val y = readNextByMode(instruction.modeFor(1))
            memory[readNext()] = op(x, y)
        }

        fun jumpIf(cond: (Int) -> Boolean) {
           val x = readNextByMode(instruction.modeFor(0))

           // Must read the target even if we don't set it.
           val loc = readNextByMode(instruction.modeFor(1))
           if (cond(x)) {
               programCounter = loc
           }
        }

        when (instruction.opcode) {
            // Add
            1 -> execBinop { x, y -> x + y }

            // Mult
            2 -> execBinop { x, y -> x * y }

            // Output
            3 -> {
                print("[Intcode] INPUT: ")
                val input = readLine()!!.trim().toInt()
                memory[readNext()] = input
            }
            
            // Input
            4 -> {
                val x = readNextByMode(instruction.modeFor(0))
                println("[Intcode] OUTPUT: $x")
            }
            
            // Jump if true
            5 -> jumpIf { it != 0 }
            
            // Jump if false
            6 -> jumpIf { it == 0 }

            // Test less than
            7 -> execBinop { x, y -> if (x < y) { 1 } else { 0 } }

            // Test equals
            8 -> execBinop { x, y -> if (x == y) { 1 } else { 0 } }

            99 -> {
                return false
            }
            else -> throw Exception("Unknown opcode: ${instruction.opcode}")
       }

       if (trace) {
           dumpState(instruction, startPc, programCounter)
       }

       return true
    }

    // Runs until the program halts
    public fun runToHalt() {
        while (step()) {}
    }

    private fun dumpState(instruction: Instruction, startPc: Int, endPc: Int) {
        val dump = memory.mapIndexed { idx, i -> 
            if (idx == startPc) {
                "{$i}"
            } else if(idx == endPc) {
                "[$i]"
            } else {
                "$i"
            }
        }.joinToString(",")
        println("${instruction.opcode} => $dump")
    }
}
