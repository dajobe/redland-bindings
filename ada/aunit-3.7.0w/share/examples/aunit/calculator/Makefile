all:
	gprbuild -p -f -Pharness

coverage:
	gprbuild -p -f -Pharness -XCOVERAGE=yes
	./test_calculator
	cd obj;	gcov ../../tested_lib/obj/*.gcda

clean:
	gprclean -Pharness
	gprclean -Ptested_lib/testlib
	-rm -rf obj
	-rm -rf tested_lib/obj
	-rm -rf tested_lib/lib
