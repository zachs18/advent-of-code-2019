#!/bin/bash

runhaskell -Wno-tabs main.hs input 1=12 2=2
echo "Day 2 Part 1: Result should be: 5482655"
read

runhaskell -Wno-tabs main.hs input find 19690720 1 2
echo "Day 2 Part 2: Result should be: 4967"
read


runhaskell -Wno-tabs main.hs ../day_5/input <<< "1"
echo "Day 5 Part 1: Result should be: 7265618"
read

runhaskell -Wno-tabs main.hs ../day_5/input <<< "5"
echo "Day 5 Part 2: Result should be: 7731427"
read



runhaskell -Wno-tabs main.hs ../day_7/input sequence 5
echo "Day 7 Part 1: Result should be: 567045"
read

runhaskell -Wno-tabs main.hs ../day_7/input loop 5
echo "Day 7 Part 2: Result should be: 39016654"
read
