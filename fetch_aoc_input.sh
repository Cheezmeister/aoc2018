#!/bin/sh

# set -ex

d=${1:-$(date +%e | perl -pe 's/ //')}

url="https://adventofcode.com/2018/day/$d/input"
curl -b session=$ADVENT_OF_CODE_TOKEN $url > aoc/input/$d.txt
