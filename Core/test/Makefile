default: run

Test: Test.hs
	ghc-6.12.3 $^ -o $@ -hide-all-packages -package base -package Truth-Shapes -package Truth-Core

%.out: %
	./$< > $@

%.diff: %.ref %.out
	diff -u $^

run: Test.diff
	@echo "Success!"

clean:
	rm -f *.o *.hi *.out Test

# switch off intermediate file deletion
.SECONDARY:
