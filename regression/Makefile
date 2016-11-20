TESTS=not_my_test001 not_my_test002 my-fun-test test011 test222 test000 test001 test002 test003 test004 test005 test006 test007 test008 test009 test010 test012 test013 test014 test015 test016 test017 test018 test019 test020 test021 test022 test023

.PHONY: check $(TESTS)

check: $(TESTS)

$(TESTS): %: %.expr
	RC_RUNTIME=../runtime ../rc.native -o  $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log
	cat $@.input | ../rc.native -i $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | ../rc.native -s $< > $@.log && diff $@.log orig/$@.log

clean:
	rm -f *.log *.s *~ $(TESTS)
