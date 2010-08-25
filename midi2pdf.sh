#!/bin/sh

function error { echo $1; exit 1; }

script=`basename $0`
file=$1
name=${file%.*}

if test -z $file; then error "Usage: $script MIDI_FILE"
elif ! test -e $file; then error "File $file doesn't exist"
fi

mkdir -p tmp
cp $file tmp/
cd tmp
midi2ly $file && mv $name-midi.ly $name.ly
lilypond $name.ly
cp $name.pdf ../

