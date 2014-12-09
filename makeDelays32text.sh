#!/usr/bin/env bash

# run this in a directory just above files-*

n=$(perl -le '$_=$ARGV[-1]; s/files-//; print;' files-*)

seq 0 $n | parallel 'receivedSnapshots2staleness32text {} < files-{}/receivedSnapshots.log > files-{}/staleness'

