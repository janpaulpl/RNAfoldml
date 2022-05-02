# About
RNAfoldml is an OCaml package that enables users to input both RNA sequences in FASTA format and a set of constraints to predict RNA secondary structure.

# Key Features
- Parsing of RNA sequences inputted in text-based FASTA format
- Implementation of folding algorithms developed by Nussinov and Zuker for free energy minimization
- Usable OCaml package with functions which generate formatted secondary structure output from 
- Online documentation for package
- Formatted output includes:
  - Description of parameters, algorithm, time, input sequence
  - Table describing RNA sequence pairing 
  - Graphical representation of RNA secondary structures


### Authors
- [Inle Bush](https://github.com/imbush)
- [Jan-Paul Ramos](https://github.com/jpvinnie)
- [Songyu Ye](https://github.com/s-ye)

---

# Getting started
1. OCaml 4.0.0+ has to be installed to ensure nothing breaks. This library uses no external dependencies except [`OUnit2` for tests](https://opam.ocaml.org/packages/ounit2/). 

2. Unzip `rnafoldml.zip` into your preferred directory.

3. Go in the unzipped `rnafoldml` folder and in the root directory, run `make build` in your command line. Build the docs with `make doc`.

4. Use the code in utop with `make utop`.
