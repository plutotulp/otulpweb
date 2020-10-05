.PHONY: all
all:	build/webclient build/server

# We'll put all builds in here. Or rather, we'll put all build
# symlinks in here.
build:
	mkdir -p build

# Poor man's PHONY.
.FORCE:

# Build any of the derivations defined in default.nix, like
# "webclient-dev" or "server".
build/*: .FORCE build
	nix-build -A $(notdir $@) -o $@

.PHONY: clean
clean:
	-rm -r build
	-rm -r otulpweb-webclient/dist-newstyle

# Serve webclient using ghcid with normal ghc and jsaddle instead of
# compiling fully with ghcjs. This is much faster, so is very handy
# during development. It is a bit flakey, though, so when that bites
# you, it is time to move on to webclient-dev-server.
.PHONY: webclient-dev-ghcid
webclient-dev-ghcid: build
	nix-shell --run run-webclient-ghcid

# Build webclient and serve statically. Does not run the
# otulpweb-server code.
.PHONY: webclient-dev-server
webclient-dev-server: build/webclient
	nix-shell -p haskellPackages.wai-app-static --run \
	'warp -d build/webclient/bin/webclient.jsexe -h localhost -p 3000'

# FIXME: There's no "dev server" target here yet, because the server
# code is basically unused at the moment; it's all client code served
# by nginx when deployed.
