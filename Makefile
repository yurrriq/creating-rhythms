.PRECIOUS: %.abc %.mid

%.abc: %.bdf
	cabal run bdrum $< 4 >$@

%.mid: %.abc
	abc2midi $< -o $@

%: examples/%.mid
	timidity -in $<

docs:
	rm -fr ./$@
	nom build .#creating-rhythms.doc --no-link --print-out-paths | \
	xargs -I% find % -name html | \
	xargs -I% cp -r % $@
	chmod -R +w $@
