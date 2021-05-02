.PHONY: all
all:	build/webclient.ghcjs.nix-result

# We'll put all build symlinks in here.
build:
	mkdir -p build

# Always outdated, always rebuilt.
.PHONY: .FORCE
.FORCE:

# Build entries in default.nix in the build directory, by adding a
# ".nix-result" suffix. Cabal test suites for the target are also run,
# if they exist.
build/%.nix-result: .FORCE build
	nix-build -A $(notdir $(basename $@)) -o $@

.PHONY: clean
clean:
	-rm -r build
	-rm -r otulpweb-webclient/dist otulpweb-webclient/dist-newstyle
	-rm -r otulpweb-common/dist otulpweb-common/dist-newstyle
	-rm -r otulpweb-server/dist otulpweb-server/dist-newstyle

# Serve webclient using ghcid with normal ghc and jsaddle instead of
# compiling fully with ghcjs. This is much faster, so is very handy
# during development. It is a bit flakey, though. When it is time to
# do a real compilation, use target build/webclient.ghc.nix-result or
# build/webclient.ghcjs.nix-result instead.
.PHONY: webclient-dev-ghcid
webclient-dev-ghcid:
	cd otulpweb-webclient && nix-shell --run run-ghcid

# Build webclient release code and serve statically at
# http://localhost:3000. Does not run the otulpweb-server code, but
# uses an ad-hoc web server instead. Hence, only useful for testing a
# static client that does not need server-side functionality.
.PHONY: webclient-dev-server
webclient-dev-server: build/webclient.ghcjs.nix-result
	nix-shell -p haskellPackages.wai-app-static --run \
	'warp -d build/webclient.ghcjs.nix-result/bin/webclient.jsexe -h localhost -p 3000'

# Run ghcid session for otulpweb-common.
.PHONY: common-dev-ghcid
common-dev-ghcid:
	cd otulpweb-common && nix-shell --run run-ghcid

# Run ghcid session for otulpweb-common's test suite.
.PHONY: common-dev-ghcid-tasty
common-dev-ghcid-tasty:
	cd otulpweb-common && nix-shell --run run-ghcid-tasty

# Run ghcid session for otulpweb-server.
.PHONY: server-dev-ghcid
server-dev-ghcid:
	cd otulpweb-server && nix-shell --run run-ghcid
