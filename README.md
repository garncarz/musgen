# Generating Music Compositions by a Computer

[<img src="https://garncarz.github.io/musgen-haskell/song.png" alt="Sample of a produced short song" align="right">](https://garncarz.github.io/musgen-haskell/song.ogg)


## Abstract

The thesis is focused on a problem of generating music by a computer, music being **random** but not very different from one created by a human, following particular **harmony rules** and not overgoing into disharmony. Basic terms from music theory are introduced, then the developed algorithm is described, both generally and in a relation to the **Haskell** implementation. The program is compared to similar software and a possible future development is suggested.

## Program

MusGen is a program generating random songs. User can specify some parameters of a demanded output, e.g. scale, tempo or a minimal duration in measures. The output is a song in **MIDI** format, which can be easily converted into a sheet music in PDF format. 


## Use

### Linux

Help: `$ ./musgen -?`

Generating a song (with example parameters): `$ ./musgen -k62 -smajor -b3 -t130 -m4 -ipop`

Converting MIDI into a sheet music in PDF: `$ ./midi2pdf.sh song.midi`

### Windows

You need to use the command line (`cmd.exe`).

Help: `musgen -?`

Generating a song (with example parameters): `musgen -k62 -smajor -b3 -t130 -m4 -ipop`


## Files

[Thesis](https://garncarz.github.io/musgen-haskell/thesis.pdf)

[Presentation](https://garncarz.github.io/musgen-haskell/prezentace.pdf) (only in Czech)
