package adventofcode.common

public class IntcodeComputer(public var memory: IntArray, private val trace: Boolean = false) {
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