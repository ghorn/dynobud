dynobud - dynamic optimization buddy

This library has a few distinct features, which may later be broken into separate packages:
* high level interface to CasADi (src/Dyno/Casadi)
* NLP modeling/solving (examples/Basic.hs, examples/BasicJ.hs)
* monadic NLP modeling DSL (examples/StaticExample.hs)
* OCP modeling/solving (examles/Glider.hs)
* monadic OCP modeling DSL (examples/OcpM.hs, examples/Rocket.hs)
* live plotter for OCP solving (examples/Plotter.hs)


The current instructions for getting started on Debian/Ubuntu:

    >> apt-get install coinor-libipopt-dev

    Install libcasadi-shared from https://github.com/casadi/casadi/releases/latest,
    first download it from that website, then:
    >> dpkg -i libcasadi-shared.deb

    >> cabal update; cabal install casadi-bindings

    >> git clone git://github.com:ghorn/dynobud.git
    >> cd dynobud
    >> cabal install --only-dependencies
    >> cabal configure
    >> cabal build

Try running the examples in dynobud/examples


you may need to do something like this on Debian/Ubuntu:

    >> cabal install alex
    >> cabal install happy
    >> cabal install gtk2hs-buildtools

    >> sudo apt-get install coinor-libipopt-dev
    >> sudo apt-get install liblapack-dev
    >> sudo apt-get install libblas-dev
    >> sudo apt-get install libglpk-dev
    >> sudo apt-get install libgl1-mesa-dev
    >> sudo apt-get install libglu1-mesa-dev
    >> sudo apt-get install freeglut3-dev
    >> sudo apt-get install libzmq3-dev
    >> sudo apt-get install libglib2.0-dev
    >> sudo apt-get install libcairo2-dev
    >> sudo apt-get install libpango1.0-dev
    >> sudo apt-get install libgtk2.0-dev
    >> sudo apt-get install libgsl0-dev


"user error: out of memory"
If you get this ^ error on OSX while using the plotting tools, your
cairo/pango/gtk may be linked to an XQuartz library.
Add "extra-lib-dirs=/usr/local/lib" (or wherever the correct libraries are)
to your .cabal/config
