build: tmp
	cp *.hs tmp/
	cd tmp && ghc --make Main
	cp tmp/Main musgen

run: build
	./musgen
	make ly
	gnome-open song.pdf
	gnome-open song.midi

tmp:
	[ -d tmp ] || mkdir tmp

ly: tmp
	cp song.midi tmp/
	cd tmp && midi2ly song.midi && mv song-midi.ly song.ly
	-cd tmp && lilypond song.ly
	cp tmp/song.pdf ./


clean:
	rm -fr *~ tmp musgen

pack: clean
	cd .. && tar -czf musgen.tar.gz musgen

