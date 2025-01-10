#!/bin/bash

printf "\n"
printf "=============== PROBLEM 2 TESTS ===============\n"
printf "BUILDING PROGRAMS\n"
cargo build


testdir=test-results
mkdir -p $testdir

printf "PERFORMING SAMPLE RUNS\n"

cmds=("cargo run --quiet --bin prob2_wc -- test-data/bruce.txt "
      "cargo run --quiet --bin prob2_wc -- test-data/gettysburg.txt.txt "
      "cargo run --quiet --bin prob2_wc -- test-data/dijkstra.txt "
      "cargo run --quiet --bin prob2_wc -- test-data/howl.txt"
      "cargo run --quiet --bin prob2_wc -- test-data/empty.txt"
      "cargo run --quiet --bin prob2_wc --"
      "cargo run --quiet --bin prob2_wc -- test-data/no-such-file.txt"
     )

# All output will go into the named file below
actual_file="$testdir/prob2_output_actual.tmp"
{
    for cmd in "${cmds[@]}"; do
        printf '>> %s\n' "$cmd"
        $cmd
        printf "\n"
    done
} >& $actual_file

expect_file="$testdir/prob2_output_expect.tmp"

cat > $expect_file <<EOF
>> cargo run --quiet --bin prob2_wc -- test-data/bruce.txt 
   2   17   91 test-data/bruce.txt

>> cargo run --quiet --bin prob2_wc -- test-data/gettysburg.txt.txt 
Couldn't open file test-data/gettysburg.txt.txt: No such file or directory (os error 2)

>> cargo run --quiet --bin prob2_wc -- test-data/dijkstra.txt 
  40  271 1633 test-data/dijkstra.txt

>> cargo run --quiet --bin prob2_wc -- test-data/howl.txt
 145 2909 17521 test-data/howl.txt

>> cargo run --quiet --bin prob2_wc -- test-data/empty.txt
   0    0    0 test-data/empty.txt

>> cargo run --quiet --bin prob2_wc --
usage: target/debug/prob2_wc <filename>

>> cargo run --quiet --bin prob2_wc -- test-data/no-such-file.txt
Couldn't open file test-data/no-such-file.txt: No such file or directory (os error 2)

EOF

printf "COMPARING EXPECTED (left) AND ACTUAL (right) OUTPUT\n"
printf "================ EXPECT ==================================         =================== ACTUAL ===============================\n"
diff -yt -bB $expect_file $actual_file
result=$?
if [[ "$result" != "0" ]]; then
    printf "EXPECTED AND ACTUAL OUTPUT DIFFER, TEST FAILURES LIKELY\n"
    printf "Examine the above side-by-side diff, look for | < > symbols\n"
    printf "in the middle which indicate differnces between the Expected\n"
    printf "and Actual output\n"
else
    printf "ok: Output matches, tests likely to pass\n"
fi
   
