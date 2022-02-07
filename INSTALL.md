# Installation

Make sure to run these commands in Ubuntu.

1. Run `make --version` to make sure that make is installed. If it is not installed, run `make install` to install make onto the system.

2. Run `opam --version` to make sure that OPAM is installed. If it is not installed, run `opam install` to install OPAM onto the system. 

3. Run `opam update` to update OPAM, allowing us to add more packages. Please be aware that this may take a while and there will be no output on the screen, so please be patient. 

4. Run `opam upgrade` to upgrade pre-exisiting packages. This is important that this is run before the ANSITerminal is installed, so that everything is updated and upgraded so that it will be compatible.

5. Run `opam install ANSITerminal odoc ocamlformat-rpc`. This will install the ANSITERMINAL, as well as odoc and the OCaml formatting that wiil help to make sure the code is properly formatted.

Make sure you are in the right file system for the following commands, within the directory of the program files. 

6. Run `make build` in order to build the files and create the dune file, so that the code will run. 

7. Run `make play` to start the game and play the battleship game. 


