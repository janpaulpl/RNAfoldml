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
  - ASCII and/or graphical representation of RNA secondary structures


### Authors
- [Inle Bush](https://github.com/imbush)
- [Jan-Paul Ramos](https://github.com/jpvinnie)
- [Songyu Ye](https://github.com/s-ye)

---

# Getting started
1. OCaml 4.0.0+ has to be installed to ensure nothing breaks. This library doesn't use any external dependencies except `OUnit2` for tests.
2. Clone this repository using your preferred method or run in command line:
```
git clone https://github.com/jpVinnie/RNAfoldml.git
```
3. In the root directory, run `make` in your command line.
4. **TO DO**