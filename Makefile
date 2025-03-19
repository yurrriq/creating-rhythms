.PRECIOUS: %.abc %.mid

%.abc: %.bdf
	cabal run bdrum $< 4 >$@

%.mid: %.abc
	abc2midi $< -o $@

%: examples/%.mid
	timidity -in $<
