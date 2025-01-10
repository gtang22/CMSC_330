#!/bin/bash

printf "\n"
printf "=============== SPELLCHECK_MAIN TESTS ===============\n"
printf "BUILDING PROGRAMS\n"
cargo build


testdir=test-results
mkdir -p $testdir

printf "PERFORMING SAMPLE RUNS\n"

cmds=("cargo -q run --bin spellcheck_main test-data/all-your-bass.txt test-data/dict-bass.txt mark"
      "cargo -q run --bin spellcheck_main test-data/all-your-bass.txt test-data/dict-bass.txt auto_show"

      "cargo -q run --bin spellcheck_main test-data/quotes.txt test-data/dict-google-10000.txt mark"
      "cargo -q run --bin spellcheck_main test-data/quotes.txt test-data/dict-google-10000.txt auto"
      "cargo -q run --bin spellcheck_main test-data/quotes.txt test-data/dict-google-10000.txt auto_show"
      "cargo -q run --bin spellcheck_main test-data/quotes.txt test-data/dict-full-english.txt auto_show"

      "cargo -q run --bin spellcheck_main test-data/gettysburg.txt test-data/dict-google-10000.txt mark"
      "cargo -q run --bin spellcheck_main test-data/gettysburg.txt test-data/dict-google-10000.txt auto"
      "cargo -q run --bin spellcheck_main test-data/gettysburg.txt test-data/dict-google-10000.txt auto_show"
      "cargo -q run --bin spellcheck_main test-data/gettysburg.txt test-data/dict-full-english.txt auto_show"
     )

# All output will go into the named file below
actual_file="$testdir/bin_output_actual.tmp"
{
    for cmd in "${cmds[@]}"; do
        printf '>> %s\n' "$cmd"
        $cmd
        printf "\n"
    done
} >& $actual_file

expect_file="$testdir/bin_output_expect.tmp"

cat > $expect_file <<EOF
>> cargo -q run --bin spellcheck_main test-data/all-your-bass.txt test-data/dict-bass.txt mark
loading dictionary test-data/dict-bass.txt
opening file test-data/all-your-bass.txt
mode: mark

CORRECTED TEXT:
All your >>BASS<< are >>BELONG<< 2 us!!


>> cargo -q run --bin spellcheck_main test-data/all-your-bass.txt test-data/dict-bass.txt auto_show
loading dictionary test-data/dict-bass.txt
opening file test-data/all-your-bass.txt
mode: auto_show

CORRECTED TEXT:
All your (bass:BASE:1) are (belong:ALL:5) 2 us!!


>> cargo -q run --bin spellcheck_main test-data/quotes.txt test-data/dict-google-10000.txt mark
loading dictionary test-data/dict-google-10000.txt
opening file test-data/quotes.txt
mode: mark

CORRECTED TEXT:
They could >>REFUDIATE<< what it is that this group is saying. They could
set the record straight.

They >>MISUNDERESTIMATED<< me.

That should be ">>POTATOE<<".

A Better >>AMERCIA<<.

Stress key ideas and >>VOCABLUARY<<.


>> cargo -q run --bin spellcheck_main test-data/quotes.txt test-data/dict-google-10000.txt auto
loading dictionary test-data/dict-google-10000.txt
opening file test-data/quotes.txt
mode: auto

CORRECTED TEXT:
They could REQUIRE what it is that this group is saying. They could
set the record straight.

They INVESTIGATED me.

That should be "POTATO".

A Better AMERICA.

Stress key ideas and VOCABULARY.


>> cargo -q run --bin spellcheck_main test-data/quotes.txt test-data/dict-google-10000.txt auto_show
loading dictionary test-data/dict-google-10000.txt
opening file test-data/quotes.txt
mode: auto_show

CORRECTED TEXT:
They could (refudiate:REQUIRE:4) what it is that this group is saying. They could
set the record straight.

They (misunderestimated:INVESTIGATED:7) me.

That should be "(potatoe:POTATO:1)".

A Better (Amercia:AMERICA:2).

Stress key ideas and (vocabluary:VOCABULARY:2).


>> cargo -q run --bin spellcheck_main test-data/quotes.txt test-data/dict-full-english.txt auto_show
loading dictionary test-data/dict-full-english.txt
opening file test-data/quotes.txt
mode: auto_show

CORRECTED TEXT:
They could (refudiate:REPUDIATE:1) what it is that this group is saying. They could
set the record straight.

They (misunderestimated:UNDERESTIMATED:3) me.

That should be "(potatoe:POTATO:1)".

A Better (Amercia:MERCIA:1).

Stress key ideas and (vocabluary:VOCABULARY:2).


>> cargo -q run --bin spellcheck_main test-data/gettysburg.txt test-data/dict-google-10000.txt mark
loading dictionary test-data/dict-google-10000.txt
opening file test-data/gettysburg.txt
mode: mark

CORRECTED TEXT:
Four score and seven years ago our fathers brought forth on this continent, a
new nation, >>CONCEIVED<< in Liberty, and dedicated to the proposition that all men
are created equal.

Now we are engaged in a great civil war, testing whether that nation, or any
nation so >>CONCEIVED<< and so dedicated, can long >>ENDURE<<. We are met on a great
battle-field of that war. We have come to >>DEDICATE<< a portion of that field, as a
final >>RESTING<< place for those who here gave their lives that that nation might
live. It is >>ALTOGETHER<< fitting and proper that we should do this.

But, in a larger sense, we can not >>DEDICATE<< -- we can not >>CONSECRATE<< -- we can
not >>HALLOW<< -- this ground. The brave men, living and dead, who >>STRUGGLED<< here,
have >>CONSECRATED<< it, far above our poor power to add or >>DETRACT<<. The world will
little note, nor long remember what we say here, but it can never forget what
they did here. It is for us the living, rather, to be dedicated here to the
>>UNFINISHED<< work which they who fought here have thus far so >>NOBLY<< advanced. It
is rather for us to be here dedicated to the great task remaining before us --
that from these >>HONORED<< dead we take increased >>DEVOTION<< to that cause for which
they gave the last full measure of >>DEVOTION<< -- that we here highly resolve that
these dead shall not have died in >>VAIN<< -- that this nation, under God, shall
have a new birth of freedom -- and that government of the people, by the people,
for the people, shall not >>PERISH<< from the earth.

Abraham Lincoln
November 19, 1863


>> cargo -q run --bin spellcheck_main test-data/gettysburg.txt test-data/dict-google-10000.txt auto
loading dictionary test-data/dict-google-10000.txt
opening file test-data/gettysburg.txt
mode: auto

CORRECTED TEXT:
Four score and seven years ago our fathers brought forth on this continent, a
new nation, CONCERNED in Liberty, and dedicated to the proposition that all men
are created equal.

Now we are engaged in a great civil war, testing whether that nation, or any
nation so CONCERNED and so dedicated, can long ENSURE. We are met on a great
battle-field of that war. We have come to DEDICATED a portion of that field, as a
final TESTING place for those who here gave their lives that that nation might
live. It is TOGETHER fitting and proper that we should do this.

But, in a larger sense, we can not DEDICATED -- we can not CONCRETE -- we can
not ALLOW -- this ground. The brave men, living and dead, who STRUGGLE here,
have CONNECTED it, far above our poor power to add or DETROIT. The world will
little note, nor long remember what we say here, but it can never forget what
they did here. It is for us the living, rather, to be dedicated here to the
FINISHED work which they who fought here have thus far so NOBLE advanced. It
is rather for us to be here dedicated to the great task remaining before us --
that from these HUNDRED dead we take increased DEVIATION to that cause for which
they gave the last full measure of DEVIATION -- that we here highly resolve that
these dead shall not have died in MAIN -- that this nation, under God, shall
have a new birth of freedom -- and that government of the people, by the people,
for the people, shall not PARISH from the earth.

Abraham Lincoln
November 19, 1863


>> cargo -q run --bin spellcheck_main test-data/gettysburg.txt test-data/dict-google-10000.txt auto_show
loading dictionary test-data/dict-google-10000.txt
opening file test-data/gettysburg.txt
mode: auto_show

CORRECTED TEXT:
Four score and seven years ago our fathers brought forth on this continent, a
new nation, (conceived:CONCERNED:2) in Liberty, and dedicated to the proposition that all men
are created equal.

Now we are engaged in a great civil war, testing whether that nation, or any
nation so (conceived:CONCERNED:2) and so dedicated, can long (endure:ENSURE:1). We are met on a great
battle-field of that war. We have come to (dedicate:DEDICATED:1) a portion of that field, as a
final (resting:TESTING:1) place for those who here gave their lives that that nation might
live. It is (altogether:TOGETHER:2) fitting and proper that we should do this.

But, in a larger sense, we can not (dedicate:DEDICATED:1) -- we can not (consecrate:CONCRETE:3) -- we can
not (hallow:ALLOW:1) -- this ground. The brave men, living and dead, who (struggled:STRUGGLE:1) here,
have (consecrated:CONNECTED:3) it, far above our poor power to add or (detract:DETROIT:2). The world will
little note, nor long remember what we say here, but it can never forget what
they did here. It is for us the living, rather, to be dedicated here to the
(unfinished:FINISHED:2) work which they who fought here have thus far so (nobly:NOBLE:1) advanced. It
is rather for us to be here dedicated to the great task remaining before us --
that from these (honored:HUNDRED:2) dead we take increased (devotion:DEVIATION:2) to that cause for which
they gave the last full measure of (devotion:DEVIATION:2) -- that we here highly resolve that
these dead shall not have died in (vain:MAIN:1) -- that this nation, under God, shall
have a new birth of freedom -- and that government of the people, by the people,
for the people, shall not (perish:PARISH:1) from the earth.

Abraham Lincoln
November 19, 1863


>> cargo -q run --bin spellcheck_main test-data/gettysburg.txt test-data/dict-full-english.txt auto_show
loading dictionary test-data/dict-full-english.txt
opening file test-data/gettysburg.txt
mode: auto_show

CORRECTED TEXT:
Four score and seven years ago our fathers brought forth on this continent, a
new nation, conceived in Liberty, and dedicated to the proposition that all men
are created equal.

Now we are engaged in a great civil war, testing whether that nation, or any
nation so conceived and so dedicated, can long endure. We are met on a great
battle-field of that war. We have come to dedicate a portion of that field, as a
final resting place for those who here gave their lives that that nation might
live. It is altogether fitting and proper that we should do this.

But, in a larger sense, we can not dedicate -- we can not consecrate -- we can
not hallow -- this ground. The brave men, living and dead, who struggled here,
have consecrated it, far above our poor power to add or detract. The world will
little note, nor long remember what we say here, but it can never forget what
they did here. It is for us the living, rather, to be dedicated here to the
unfinished work which they who fought here have thus far so nobly advanced. It
is rather for us to be here dedicated to the great task remaining before us --
that from these honored dead we take increased devotion to that cause for which
they gave the last full measure of devotion -- that we here highly resolve that
these dead shall not have died in vain -- that this nation, under God, shall
have a new birth of freedom -- and that government of the people, by the people,
for the people, shall not perish from the earth.

Abraham Lincoln
November 19, 1863


EOF

printf "\n"
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
   
