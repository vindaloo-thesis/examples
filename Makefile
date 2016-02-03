compile_io = idris -p effects -p contrib idris/$(1)_IO.idr -i ../idris-se/lib -i idris  -o dist/$(1) --no-partial-eval
compile_sio = idris -p effects -p contrib idris/$(1)_SIO.idr -i ../idris-se/lib -i idris -o dist/$(1).se --no-partial-eval --total --codegen se

all: io sio

io:
	$(call compile_io,Bank)
	$(call compile_io,Bank2)
	$(call compile_io,Bank3)
	$(call compile_io,Bank4)
	$(call compile_io,RPS)

sio:
	$(call compile_sio,Bank)
	$(call compile_sio,Bank2)
	$(call compile_sio,Bank3)
	$(call compile_sio,Bank4)
	$(call compile_sio,RPS)

bank_sio:
	$(call compile_sio,Bank)

bank2_sio:
	$(call compile_sio,Bank2)

bank3_sio:
	$(call compile_sio,Bank3)

rps_sio:
	$(call compile_sio,RPS)

bank_io:
	$(call compile_io,Bank)

bank2_io:
	$(call compile_io,Bank2)

bank3_io:
	$(call compile_io,Bank3)

rps_io:
	$(call compile_io,RPS)

clean:
	rm dist/*
	rm **/**.ibc
