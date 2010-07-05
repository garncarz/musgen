build:
	ghc --make Main
	cp Main musgen

run: build
	./musgen
	make ly
	gnome-open export.pdf
	gnome-open export.midi

tmp:
	[ -d tmp ] || mkdir tmp

ly: tmp
	cp export.midi tmp/
	cd tmp && midi2ly export.midi && mv export-midi.ly export.ly
	-cd tmp && lilypond export.ly
	cp tmp/export.pdf ./


clean:
	rm -fr *~ *.hi *.o tmp Main musgen

