package adventofcode.day04

fun main(args: Array<String>) {
    if (args.size < 2) {
        System.err.println("Usage: adventofcode day04 <START> <END>")
        System.exit(1)
    }

    val start = args[0].toInt()
    val end = args[1].toInt()

    println("Computing passwords in range $start - $end")

    val part1Passwords = (start..end).filter { isValidPassword(it, true) }
    println("[Part 1] Number of valid passwords: ${part1Passwords.size}")
    val part2Passwords = (start..end).filter { isValidPassword(it, false) }
    // for(invalidPassword in part1Passwords.minus(part2Passwords)) {
    //     println("[Part 2] Invalid: $invalidPassword")
    // }
    // for(validPassword in part2Passwords) {
    //     println("[Part 2] Valid: $validPassword")
    // }
    println("[Part 2] Number of valid passwords: ${part2Passwords.size}")
}

fun Int.iterateDigits(): List<Int> {
    var number = this
    return generateSequence {
        if (number == 0) {
            null
        } else {
            val digit = number % 10
            number = number / 10
            digit
        }
    }.toList().reversed()
}

fun isValidPassword(candidate: Int, allowMoreThanTwoDigitSequence: Boolean): Boolean {
    var last = -1
    var activeSequenceLength = 0
    var hasValidSequence = false
    for (digit in candidate.iterateDigits()) {
        if (last >= 0) {
            if (last > digit) {
                // Digits must be ascending
                return false;
            }

            if (allowMoreThanTwoDigitSequence) {
                if(last == digit) {
                    hasValidSequence = true
                }
            }
            else if (last != digit) {
                if (activeSequenceLength == 2) {
                    // Found a sequence of exactly two digits
                    hasValidSequence = true
                }
                activeSequenceLength = 0
            }
        }
        activeSequenceLength += 1
        last = digit
    }

    return hasValidSequence || activeSequenceLength == 2
}
