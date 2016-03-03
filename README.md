## dynobud - your dynamic optimization buddy

[![Build Status](https://secure.travis-ci.org/ghorn/dynobud.png?branch=master)](http://travis-ci.org/ghorn/dynobud)

This library has a few distinct features:
* high-level, strongly-typed interface to CasADi
* NLP modeling/solving (examples/EasyNlp.hs, examples/BasicNlp.hs, examples/SofaExpando.hs)
* OCP modeling/solving (examles/Glider.hs, examples/DaeColl.hs, examples/Rocket.hs, etc)
* live plotter for viewing OCP convergence (examples/Dynoplot.hs)

This package is built on top of CasADi (www.casadi.org).
You will have to install the CasADi C++ libraries and the casadi-bindings haskell package.
See http://hackage.haskell.org/package/casadi-bindings for instructions.
Installing ipopt is also highly recommended if you want to solve NLPs (`apt-get install coinor-libipopt-dev` if you're lucky).
If you are solving large scale problems, I recommend using the HSL linear solvers, which are available for free for academic users.

This library is fairly unstable, as it is in active use and development.
For usage, consult the examples folder.

To install:

    >> cabal update
    >> cabal install dynobud

casadi-bindings will probably fail, re-read casadi-bindings instructions

To install dependencies, you may need to do something like this:

    >> cabal install alex
    >> cabal install happy
    >> cabal install gtk2hs-buildtools

    >> sudo apt-get install coinor-libipopt-dev liblapack-dev libblas-dev libglpk-dev libgl1-mesa-dev libglu1-mesa-dev freeglut3-dev libzmq3-dev libglib2.0-dev libcairo2-dev libpango1.0-dev libgtk-3-dev libgsl0-dev

To build dynobud from source

    >> git clone git://github.com:ghorn/dynobud.git
    >> cd dynobud
    >> cabal install --only-dependencies            # without examples
    >> cabal install --only-dependencies -fexamples # with examples
    >> cabal configure
    >> cabal build

Try running the examples in dynobud/examples.

    >> cabal configure -fexamples
    >> cabal build
    >> dist/build/rocket/rocket

OSX issue:

    "user error: out of memory"

If you get this ^ error on OSX while using the plotting tools, your
cairo/pango/gtk may be linked to an XQuartz library.
Add "extra-lib-dirs=/usr/local/lib" (or wherever the correct libraries are)
to your .cabal/config and re-install haskell bindings to cairo/pango/gtk/etc
