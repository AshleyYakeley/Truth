default: run

Test: Test.hs
	ghc $^ -o $@ -package Changes

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
