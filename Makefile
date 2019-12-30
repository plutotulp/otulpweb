.PHONY: all
all:	webclient server

build:
	mkdir -p build

.PHONY: webclient
webclient: build
	nix-build -A webclient -o build/webclient

.PHONY: server
server: build
	nix-build -A server -o build/server

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

# Build webclient and serve with a very basic python web server.
.PHONY: webclient-dev-server
webclient-dev-server: webclient
	cd build/webclient/bin/webclient.jsexe && \
	python2 -m SimpleHTTPServer 8080
