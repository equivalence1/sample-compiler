TESTS=test1 test2 test3 test4
TESTS_GCC=search_tree

.PHONY: check $(TESTS) $(TESTS_GCC)

check: $(TESTS_GCC) $(TESTS)

$(TESTS): %: %.expr
	RC_RUNTIME=../runtime ../rc.native -o  $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log
#	cat $@.input | ../rc.native -i $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | ../rc.native -s $< > $@.log && diff $@.log orig/$@.log

$(TESTS_GCC): %: %.expr
	RC_RUNTIME=../runtime ../rc.native -o  $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log

clean:
	rm -f *.log *.s *~ $(TESTS) $(TESTS_GCC)
