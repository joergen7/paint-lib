PKG         := paint-lib
DEPS-FLAGS  := --check-pkg-deps --unused-pkg-deps
JOBS        := 16

.PHONY: all
all: setup

.PHONY: install
install:
	raco pkg install -j $(JOBS) --auto

.PHONY: remove
remove:
	raco pkg remove -j $(JOBS) $(PKG)

.PHONY: setup
setup:
	raco setup -j $(JOBS) --tidy $(DEPS-FLAGS) --pkgs $(PKG)

.PHONY: clean
clean:
	raco setup -j $(JOBS) --fast-clean --pkgs $(PKG)
	$(RM) -r coverage
	$(RM) -r `find ./ -type d -name compiled`

.PHONY: test
test:
	raco test -j $(JOBS) -x -p $(PKG)

.PHONY: cover
cover:
	raco cover .
