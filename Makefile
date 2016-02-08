compile_io = idris -p effects -p contrib idris/$(1)_IO.idr -i ../idris-se/lib -i idris  -o dist/$(1) --no-partial-eval
compile_eio = idris -p effects -p contrib idris/$(1)_EIO.idr -i ../idris-se/lib -i idris -o dist/$(1).se --no-partial-eval --total --codegen se

all: io eio

io:
	$(call compile_io,Bank)
	$(call compile_io,Bank2)
	$(call compile_io,Bank3)
	$(call compile_io,Bank4)
	$(call compile_io,RPS)

eio:
	$(call compile_eio,Bank)
	$(call compile_eio,Bank2)
	$(call compile_eio,Bank3)
	$(call compile_eio,Bank4)
	$(call compile_eio,RPS)

mul_eio:
	$(call compile_eio,Mul)

bank_eio:
	$(call compile_eio,Bank)

bank2_eio:
	$(call compile_eio,Bank2)

bank3_eio:
	$(call compile_eio,Bank3)

bank4_eio:
	$(call compile_eio,Bank4)

rps_eio:
	$(call compile_eio,RPS)

namecoin_eio:
	$(call compile_eio,Namecoin)

bank_io:
	$(call compile_io,Bank)

bank2_io:
	$(call compile_io,Bank2)

bank3_io:
	$(call compile_io,Bank3)

bank4_io:
	$(call compile_io,Bank4)

rps_io:
	$(call compile_io,RPS)

namecoin_io:
	$(call compile_io,Namecoin)

clean:
	rm dist/*
	rm **/**.ibc
