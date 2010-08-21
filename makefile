DATE = `date +%Y-%m-%d`
BUILD_DATE = "buildDate = \"$(DATE)\""

build: tmp var
	ghc --make Main -outputdir tmp -o musgen -O2

run: build
	./musgen
	$(MAKE) ly

open:
	firefox song.midi &
	firefox song.pdf &

tmp:
	mkdir tmp

ly: tmp
	cp song.midi tmp/
	cd tmp && midi2ly song.midi && mv song-midi.ly song.ly
	-cd tmp && lilypond song.ly
	cp tmp/song.pdf ./


todo: hlint
	find . -name \*.hs -exec grep TODO {} -Hn \;

hlint:
	hlint *.hs -c -i "Use head"

var:
	@(cat Var.hs 2> /dev/null | grep $(BUILD_DATE) -q) || \
		(echo "module Var where" > Var.hs && \
		echo $(BUILD_DATE) >> Var.hs)

clean:
	rm -fr *~ tmp musgen Var.hs

pack: clean
	cd .. && tar -czf musgen-$(DATE).tar.gz musgen --exclude=.svn

