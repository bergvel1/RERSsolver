# RERSsolver

This project is an experiment in code reachability analysis, built to run on the [Rigorous Examination of Reactive Systems (RERS)](http://www.rers-challenge.org/) sequential reachability problems. By using a [weakest-precondition](https://en.wikipedia.org/wiki/Predicate_transformer_semantics#Weakest_preconditions) search over the control flow graph of a sequential C program, we are able to determine code reachability by calculating if at least one valid path exists from some code fragment back to the head of the control flow graph. From initial performance testing, this search method appears to offer time and space complexities that are comparable to or even superior to those of some existing reachability analysis methods, such as symbolic execution and fuzzing.

A detailed technical writeup may be found [here](writeup.pdf).

## Getting Started

This project was developed and tested on Mac OS X El Capitan, but it should also work on Linux. For use with OCaml 4.04.0.

### Prerequisites

This project depends on:

* [Parmap](http://www.dicosmo.org/code/parmap/)
* [OCamlyices](http://micdel.fr/ocamlyices.html)
* [Alt-Ergo Zero](http://cubicle.lri.fr/alt-ergo-zero/) (optional)

### Installing

Parmap is most easily installed through OPAM:

```
opam install parmap
```

The installation process for OCamlyices is a bit more involved, especially on Mac OS X. If you are using Linux, you should be able to follow the instructions as given [here](http://micdel.fr/ocamlyices.html). If you are using Mac OS X, do the following:

You will need to install the GNU versions of several programs exist under the same name on Mac OS X, but behave slightly differently to their GNU counterparts. Luckily, these programs may all be easily installed via Homebrew: 

```
brew install coreutils
brew install gcc
brew install gnu-sed
```

Next, you will need to download [Yices](http://yices.csl.sri.com/old/download-yices1-full.html) and [OCamlyices](https://github.com/polazarus/ocamlyices). Make sure you download the *statically linked* version of Yices.

In order to build Yices and OCamlyices, you will need to make sure that their installation scripts use the programs you just installed with Homebrew, and not the Mac OS X programs with the same names. To do this, modify your .bash_profile file with the following two lines:

```
PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH”
```

**NOTE:** These paths might be slightly different depending on how your Homebrew is set up. Also, This might cause stability issues with native Mac programs, so it’s probably safest to remove these lines once you’re done installing Ocamlyices.

You should now be ready to install Yices. Navigate to the OCamlyices directory and run the installation script, passing the Yices tarball you downloaded as an argument:

```
cd ocamlyices
./install-yices.sh ../yices-1.0.40-x86_64-apple-darwin10.8.0-static-gmp.tar.gz
```

Next, run the configure script, specifying the correct compilers to use (i.e. the ones you got from the `gcc` Homebrew package):

```
./configure CC=gcc-7 CXX=g++-7
```

Finally, run:

```
make
make install
```

OCamlyices should now be visible in ocamlfind, and ready to use. 

**OPTIONAL:** In order to use Alt-Ergo Zero, simply place the AEZ directory into the root directory of this project and follow the [installation instructions](http://cubicle.lri.fr/alt-ergo-zero/).

Once you have installed the necessary dependencies, you just need to run `make` in the root directory of RERSsolver to finish the build process:

```
cd RERSsolver
make
```

### Usage

RERSsolver is run on the command line with:

```
./main
```

You will be asked to provide a path to a C source file to search, as well as a search depth. The search depth refers to the maximum input length being searched for (i.e. the number of times a search trace may pass through the node corresponding to the `scanf` line in the RERS problems). Note that terminal nodes (corresponding to VERIFIER_error calls) which are marked unreachable at one search depth may be reachable at a different, higher search depth. For this reason, it usually takes some experimentation to find a proper search depth. 
