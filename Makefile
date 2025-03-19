.PRECIOUS: %.abc %.mid

%.abc: %.bdf
	./bdrum $< 4 >$@

%.mid: %.abc
	abc2midi $< -o $@

%: examples/%.mid
	timidity -in $<
