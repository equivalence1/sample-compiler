TESTS=$(basename $(wildcard generated*.expr))

.PHONY: check $(TESTS)

check: make_src $(TESTS)

make_src:
	cd ../.. && make -f Makefile.ob clean && make -f Makefile.ob

$(TESTS): %: %.expr
	RC_RUNTIME=../../runtime ../../rc.native -o $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log
#	RC_RUNTIME=../../runtime ../../rc.native -s $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log
	cat $@.input | RC_RUNTIME=../../runtime ../../rc.native    $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | RC_RUNTIME=../../runtime ../../rc.native -s $< > $@.log && diff $@.log orig/$@.log

clean:
	rm -f *.log *.s *~ $(TESTS)
