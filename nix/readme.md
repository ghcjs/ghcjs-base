Hix is still a little experimental, but it already provides an easy way to get
a nix shell for building `ghcjs-base` without the need for a lot of nix code
in this repo.

Install hix:

```
nix-env -iA hix -f https://github.com/input-output-hk/haskell.nix/tarball/hkm/hix
```

Start a nix shell using hix:

```
git clone https://github.com/ghcjs/ghcjs-base.git
cd ghcjs-base
hix-shell
```

In the shell build the library run:

```
js-unknown-ghcjs-cabal build all
```

