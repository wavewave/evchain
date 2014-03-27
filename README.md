evchain
=======

Installation
------------


For installation, some knowledge about haskell toolchain
(`cabal-install` for example) is quite needed although it is not crazily
difficult once getting used to it. For you to just start, you need to
have `ghc-7.4` or higher and `cabal-install` installed on your system, and
clone the following packages from http://github.com/hep-platform :

`HEPUtil`
`conduit-util`
`LHEParser`
`LHE-sanitizer`
`madgraph-auto`
`madgraph-auto-model`
`pipeline-eventgen`
`evchain`

and at each directory, you need to run `cabal install`
After evchain is installed, you need to write a script in haskell for
using evchain.
One actual example of such script is
 https://github.com/wavewave/lhc-analysis-collection/blob/master/exe/2013-07-20-XUDD.hs
