all:
	make clean -f Makefile.ob && make -f Makefile.ob

.PHONY: clean

clean:
	make clean -f Makefile.ob
