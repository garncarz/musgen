DATE = `date +%Y-%m-%d`
VERSION_DATE = "versionDate = \"$(DATE)\""

build: tmp
	ghc --make Main -outputdir tmp -o musgen -O2

tmp:
	mkdir tmp

var:
	@grep -q $(VERSION_DATE) Var.hs 2> /dev/null || \
		(echo "module Var where" > Var.hs && \
		echo $(VERSION_DATE) >> Var.hs)


todo:
	find . -name \*.hs -exec grep TODO {} -Hn \;

hlint:
	hlint *.hs -c -i "Use head"


tex: tmp
	sed "s/\\\the\ /\\\article\ /g" thesis.lytex > tmp/thesis.lytex
	cd tmp && lilypond-book --pdf thesis.lytex
	cd tmp && pdflatex thesis.tex
	cp tmp/thesis.pdf ./

graphs1:
	mkdir -p tmp/import
	cp *.hs tmp/import/
	for file in `ls *.hs`; do \
		name=$${file%.*}; \
		mkdir -p tmp/$$name; \
		cp $$file tmp/$$name/; \
		for import in `grep import $$file | sed "s/import //g"`; do \
			if [ -f $$import.hs ]; then \
				cp $$import.hs tmp/$$name/; \
			fi; \
		done; \
	done

graphs2:
	SourceGraph tmp/import/Main.hs
	dot tmp/import/SourceGraph/graphs/imports.dot -Tsvg -Grankdir=LR \
		-o tmp/imports.svg
	svg2pdf tmp/imports.svg tmp/imports.pdf
	for file in `ls *.hs`; do \
		name=$${file%.*}; \
		SourceGraph tmp/$$name/$$file; \
		dot tmp/$$name/SourceGraph/graphs/codeCW.dot -Tsvg -Grankdir=LR \
			-o tmp/$$name.svg; \
		svg2pdf tmp/$$name.svg tmp/$$name.pdf; \
	done


clean:
	rm -fr *~ tmp musgen

pack: clean
	cd .. && tar -czf musgen-$(DATE).tar.gz musgen --exclude=.svn --exclude=www

