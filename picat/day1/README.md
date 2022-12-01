I don't usually do Advent of Code, but I've found it's a really good way to explore an unfamiliar language. This year, I've decided to try some of the problems in Picat, a language I'd like to become more comfortable with.

I did the problem first in Clojure and then in Picat.

Clojure allows you to express the problem as a simple data-manipulation pipeline: read the file as a string, split the string at the blank lines, split each block at ends of lines, read each string as a number, add up the numbers from each block, find the max for part 1 or sort in descending order to add up the top 3 for part 2. When doing part 1 you don't really know what to expect from part 2, so this pipeline also gives you a lot of flexibility to quickly solve the second part.

I couldn't figure out how to do any such simple manipulation in Picat. I think part of the issue is Picat's lack of support for regexes, so as far as I know, there's no way to split the string at "\n\n", and there don't seem to be any higher-order functions for quick partitioning. Since I had already seen part 2 when doing the Clojure version, I ended up leveraging my knowledge that both parts 1 and 2 only require the sum of numbers in each block. This knowledge allowed me to combine the traversal of the list of lines and the creation of a list of sums into one pass, which isn't really how you'd solve it if you didn't know what was coming in part 2, but it simplified the task of writing it in Picat.

As far as I know, Picat's arrays have a fixed-length, so the most efficient way to build up a list of unknown length is to create a list with an unbound tail variable, and keep binding that tail to a cons cell with the next element and a new unbound tail. I did this in a tail-recursive function that takes the list of text lines, the current index of which line we're looking at, an accumulator holding the calories we've seen so far in this block, and the unbound tail we need to append to.

I got tripped up initially forgetting that indexing is 1-based in Picat, and it took me a while to debug the error message I was getting.

I'd love to know if there's a more elegant pipeline-y way to do this in Picat. Also, I think I've seen some alternative syntaxes that are more concise for if-then-else constructs, but I forget how they work. My overall workflow for editing and debugging Picat is super clunky, so any tips there are welcome as well.

You can compare with my Clojure code here:
https://github.com/Engelberg/aoc22/blob/main/clojure/aoc22/src/aoc22/day1/core.clj

Note that the Clojure code has a long header of libraries I typically import for most programming puzzles, even though none were utilized here.
