default: run

Test: Test.hs
	ghc $^ -o $@ -package Changes

%.out: %
	./$< > $@

%.diff: %.ref %.out
	diff -u $^

run: Test.diff

