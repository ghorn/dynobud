dynobud - dynamic optimization buddy

This library has a few distinct features, which may later be broken into separate modules:
* high level interface to CasADi
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
    >> cabal configure --disable-library-profiling
    >> cabal build

Try running the examples in dynobud/examples
