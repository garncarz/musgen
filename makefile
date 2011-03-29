DATE = `date +%Y-%m-%d`
VERSION_DATE = "versionDate = \"$(DATE)\""
graph = mkdir -p tmp/$(1); \
	cp -t tmp/$(1) $(3) $(4) $(5); \
	SourceGraph tmp/$(1)/$(3); \
	svg2pdf tmp/$(1)/SourceGraph/graphs/$(2).svg tmp/$(1).pdf


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

graphs: tmp
	$(call graph,interpretation,codeCluster,Interpretation.hs,InterpretationTechniques.hs)


clean:
	rm -fr *~ tmp musgen

pack: clean
	cd .. && tar -czf musgen-$(DATE).tar.gz musgen --exclude=.svn --exclude=www

