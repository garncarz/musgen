#!/bin/bash

[ -d tmp ] || mkdir tmp
midi2ly export.midi
mv export-midi.ly tmp/export.ly
cd tmp
lilypond export.ly
mv export.pdf ../export.pdf

