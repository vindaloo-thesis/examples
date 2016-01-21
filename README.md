Samle Idris Ethereum smart contracts
------------------------------------
Sample contracts to show how to use [Idris](http://www.idris-lang.org/) as a smart contract language for the [Ethereum](https://ethereum.org/) platform.


Prerequisites
-------------
* [Idris](https://github.com/idris-lang/Idris-dev/) (tested against 0.9.20.1) with FFI support
* [Serpent back end for Idris](https://github.com/vindaloo-thesis/idris-se)
* [Serpent](https://github.com/ethereum/serpent) (tested against 2.0.2) or [pyethereum]

For compiling to actual EVM code (not yet fully possible due to back end output size), you need either:

* [pyethereum](https://github.com/ethereum/pyethereum) (for running Serpent code in a simulator), or
* An Ethereum client like [AlethZero](https://github.com/ethereum/alethzero) to compile the LLL code


Building
--------
For example, building the Rock-Paper-Scissors sample. Assuming `idris-se` and `examples` are checked out in the same directory.

Compiling to Serpent and then to LLL:

    idris -p effects -p contrib idris/RPS_SIO.idr -i idris/ -i ../idris-se/lib --codegen se --interface -o rps.se --no-partial-eval
    serpent compile_to_lll rps.se > rps.lll

Run using the test runner (should work):
    
    idris -p effects -p contrib idris/RPS_IO.idr -i ../idris-se/lib -i idris  -o rps --no-partial-eval && ./rps


