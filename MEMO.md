# This file contains the info necessary to mantain this package.


Creating the default cabal file.

'''
nix-shell --pure -p ghc cabal-install --run "cabal init"
'''

Regenerating default.nix

'''
nix-shell --pure -p cabal2nix --run "cabal2nix ." > default.nix

'''

Building the package

```nix-build release.nix```

Opening a shell with nix enabled:


```nix-shell --attr env release.nix```


Obtained from <https://maybevoid.com/posts/2019-01-27-getting-started-haskell-nix.html>
