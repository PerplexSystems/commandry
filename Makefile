# SPDX-FileCopyrightText: 2025 Victor Freire <https://gluer.org>
#
# SPDX-License-Identifier: MIT

MLTON = mlton
SMLFMT = smlfmt

LUNARML = lunarml
LUA = lua

SRC_ROOT = lib/github.com/PerplexSystems/commandry

PATHMAP = smlpm.pathmap

SOURCES = lib.mlb \
	$(wildcard $(SRC_ROOT)/*.sml) \
	$(wildcard $(SRC_ROOT)/*.mlb) \
	$(wildcard $(SRC_ROOT)/**/*.sml) \
	$(wildcard $(SRC_ROOT)/**/*.mlb)

TESTS_SOURCES = $(SOURCES) \
	$(wildcard tests/*.sml) \
	$(wildcard tests/*.mlb) \
	$(wildcard tests/**/*.sml) \
	$(wildcard tests/**/*.mlb) \
	$(wildcard docs/**/*.sml) \
	$(wildcard docs/**/*.mlb)

all: build/mlton build/lunarml test

build:
	mkdir -p $@/test

$(PATHMAP):
	smlpm sync --dev

build/mlton: $(PATHMAP) $(SOURCES) build 
	$(MLTON) -mlb-path-map $(PATHMAP) -output $@ lib.mlb

build/lunarml: $(PATHMAP) $(SOURCES) build
	$(LUNARML) --mlb-path-map $(PATHMAP) -B $$LUNARML_LIB compile --output $@ lib.mlb

build/test/mlton: $(PATHMAP) $(TESTS_SOURCES) build
	$(MLTON) -mlb-path-map $(PATHMAP) -output $@ tests/tests.mlb

build/test/lunarml: $(PATHMAP) $(TESTS_SOURCES) build
	$(LUNARML) --mlb-path-map $(PATHMAP) -B $$LUNARML_LIB compile --output $@ tests/tests.mlb

test: build/test/mlton build/test/lunarml
	./build/test/mlton
	$(LUA) ./build/test/lunarml

format: $(SOURCES) $(TEST_SOURCES)
	$(SMLFMT) --force **/*.mlb

clean:
	rm -rf build

.PHONY: all format clean test
