compile_io = idris -p effects -p contrib idris/$(1)_IO.idr -i ../idris-se/lib -i idris  -o dist/$(1) --no-partial-eval --total
io:
	$(call compile_io,RPS)
	$(call compile_io,Bank)
