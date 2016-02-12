#!/bin/bash

for i in step{1,2,3,4}
do
    hs=$i.hs
    commands="`cat $hs`"

    echo "$commands"
    read next
    echo -e ":load scratchpad\n$commands" | ghci

done
#:load scratchpad
#quickCheck prop_isPowerDetectsExponents'
#quickCheck prop_isPowerIsFalseOtherwise'


