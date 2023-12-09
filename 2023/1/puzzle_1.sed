#!/bin/sed -nf

s/^[^0-9]*\([0-9]\).*\([0-9]\)[^0-9]*$/\1\2/p
t
s/^[^0-9]*\([0-9]\)[^0-9]*$/\1\1/p
T

# Then pipe through `awk '{ sum += $1 } END { print sum }'`
