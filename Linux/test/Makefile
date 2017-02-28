default: run

Test: Test.hs
	ghc-6.10.4 $^ -o $@ -hide-all-packages -package base -package bytestring -package Truth-Shapes -package Truth-Core -package Truth-Linux

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
