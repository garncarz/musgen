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


clean:
	rm -fr *~ tmp musgen

pack: clean
	cd .. && tar -czf musgen-$(DATE).tar.gz musgen --exclude=.svn --exclude=www

