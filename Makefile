# This Makefile attempts to follow the best-practice set out by Alexis King
# in "An opinionated guide to Haskell in 2018" where we build developer tooling
# as part of the project environment rather than globally. This ensures that
# tools like `ghcmod` are using the same version of GHC as our target runtime to
# get the most relevant results.
#
# https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/
BIN_PATH=$(shell stack path | grep bin-path | awk -F\  '{ print $$2 }' | head -n1)
export PATH := $(BIN_PATH):$(PATH)

.PHONY: test

clean:
	@stack clean && rm **/*.cabal

# This should only need to be done once per developer machine.
setup: clean
	stack build --copy-compiler-tool ghc-mod stylish-haskell hlint apply-refact

_HLINT=hlint --refactor --refactor-options -i {} \;
hlint:
	@find {producer,consumer}/{src,test} -name "*.hs" -exec $(_HLINT)

_STYLISH=stylish-haskell -i {} \;
stylish-haskell:
	@find {producer,consumer}/{src,test} -name "*.hs" -exec $(_STYLISH)

test:
	stack test --test-arguments "--color always"
