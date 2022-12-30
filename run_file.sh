#!/usr/bin/env bash

# test_file="./test.hs"
# test_file="./chapter_8_exercise.hs"
test_file="./countdown_problem.hs"

echo $test_file | entr sh -c "runhaskell ${test_file}"