# Advent of Code 2019

Implemented in [Kotlin](https://kotlinlang.org/).

As always, remember that any input files here are unique to my account, so you need your own inputs. And the fun is in the challenge, so please try the challenge yourself before looking at my solutions!

If you have ideas or thoughts on how to improve my solutions, I'd be happy to take a look and learn more about Kotlin. Please feel free to send PRs. I may not merge them, but I promise I'll try to read any PRs I get!

## Running

Unlike other years, all days are built into the same app. The gradle wrapper is included so as long as you have Java, I think the rest will come automatically (including Kotlin?).

Run `.\gradlew build` to compile, then run `java -jar .\build\libs\aoc2019.jar <ARGS>`. The first argument must be the number of the day to run (`1`, `day1`, `day01` are all accepted formats). The remaining args are passed to that day's "main" as-is.