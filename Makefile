.PHONY: all
all:	build/otulpweb-deployment.nix-result

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

# Run ghcid session for otulpweb-webclient. This is good for spotting
# compilation errors quickly.
.PHONY: webclient-dev-ghcid
webclient-dev-ghcid:
	cd otulpweb-webclient && nix-shell --run run-ghcid

# Like the above, but additionally serves the client at localhost
# whenever it builds successfully. Does not run the normal backend
# server, though, so any interactivity with backend will not work.
.PHONY: webclient-dev-ghcid-server
webclient-dev-ghcid-server:
	cd otulpweb-webclient && nix-shell --run run-ghcid-main

# Run ghcid session for otulpweb-common.
.PHONY: common-dev-ghcid
common-dev-ghcid:
	cd otulpweb-common && nix-shell --run run-ghcid

# Run ghcid session for otulpweb-common's test suite.
.PHONY: common-dev-ghcid-tasty
common-dev-ghcid-tasty:
	cd otulpweb-common && nix-shell --run 'run-ghcid-main test:tasty'

# Run ghcid session for otulpweb-server.
.PHONY: server-dev-ghcid
server-dev-ghcid:
	cd otulpweb-server && nix-shell --run run-ghcid

# Run ghcid session for otulpweb-server, including starting the
# server.
.PHONY: server-dev-ghcid
server-dev-ghcid-server:
	cd otulpweb-server && nix-shell --run run-ghcid-main
