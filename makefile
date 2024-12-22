
build:
	cabal build

run:
	@printf "\n/= Running checks, testing and running =\\"
	@printf "\n|=======================================|\n|\n"
	@cabal check                                  \
		&& printf "|\n|\\__ Checks succeeded\n|\n"
	@cabal test --verbose=0                       \
		&& printf "|\n|\\__ Tests succeeded\n|\n"   \
		|| printf "|\n|\\__ Tests failed\n|\n"
	@cabal run  --verbose=0                       \
		&& printf "|\n\\___ Running succeeded\n" \
		|| printf "|\n\\___ Running failed\n"

