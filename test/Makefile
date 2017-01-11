TESTS=test1 test2 test3 test4

.PHONY: check $(TESTS)

check: $(TESTS)

$(TESTS): %: %.expr
	RC_RUNTIME=../runtime ../rc.native -o  $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log
#	cat $@.input | ../rc.native -i $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | ../rc.native -s $< > $@.log && diff $@.log orig/$@.log

clean:
	rm -f *.log *.s *~ $(TESTS)
