#!/bin/bash

for i in step{1,2,4,3,5}
do
    hs=$i.hs
    code=$i.code
    commands="`cat $hs`"
    clear
    echo "Code:"
    cat $code
    echo
    read next
    echo "Tests"
    echo "$commands"
    echo
    read next
    echo -e ":load scratchpad\n$commands" | ghci
    read next

done
#:load scratchpad
#quickCheck prop_isPowerDetectsExponents'
#quickCheck prop_isPowerIsFalseOtherwise'


