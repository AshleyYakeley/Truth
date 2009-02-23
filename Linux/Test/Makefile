default: run

Test: Test.hs
	ghc $^ -o $@ -package Changes -package Changes-Linux

%.out: %
	./$< > $@

%.diff: %.ref %.out
	diff -u $^

run: Test.diff
	@echo "Success!"

clean:
	rm -f *.out *.o *.hi Test

# switch off intermediate file deletion
.SECONDARY:
