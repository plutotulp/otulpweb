.PHONY: all
all:	webclient server

build:
	mkdir -p build

# The gcid_webclient_jsaddle bash function is defined in shell.nix.
.PHONY: webclient-dev
webclient-dev: build
	nix-shell --run gcid_webclient_jsaddle

.PHONY: webclient
webclient: build
	nix-build -A webclient -o build/webclient

.PHONY: server
server: build
	nix-build -A webclient -o build/server

.PHONY: clean
clean:
	-rm -r build
