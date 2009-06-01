default: run

Test: Test.hs
	ghc $^ -o $@ -package Changes -package Changes-Linux

%.out: %
	./$< > $@ &
	sleep 5
	echo yyzz >> somefile
	echo xfxfx > somefile
	echo pqpr >> somefile
	sleep 10

%.diff: %.ref %.out
	diff -u $^

run: Test.diff
	@echo "Success!"

clean:
	rm -f *.out *.o *.hi Test

# switch off intermediate file deletion
.SECONDARY:
