GPRBUILD  = gprbuild
GPRCLEAN  = gprclean

.PHONY: all test force

all: test

RTS =
TARGET =

ifeq ($(RTS),)
   RTS = full
   RTS_CONF =
else
   RTS_CONF = --RTS=$(RTS)
endif

ifeq ($(TARGET),)
   TARGET = native
   TARGET_CONF =
else
   TARGET_CONF = --target=$(TARGET)
endif

CONF_ARGS = $(TARGET_CONF) $(RTS_CONF)

ifeq ($(OS),Windows_NT)
 ifeq ($(TARGET),native)
   exeext=.exe
 endif
endif

ifeq ($(findstring vxworks,$(TARGET)),vxworks)
   exeext=.out
endif

RUN=
ifeq ($(TARGET),powerpc-elf)
   RUN=./support/run-ppc-elf
endif

VARIANT=
ifeq ($(TARGET),powerpc-elf)
   VARIANT=-XVARIANT=powerpc-elf-psim
endif
ifeq ($(TARGET),lmp-elf)
   VARIANT=-XVARIANT=lmp-elf
endif

ifneq ($(findstring aunit.gpr,$(wildcard ../lib/gnat/*.gpr)),)
   PROJECT_PATH_ARG = GPR_PROJECT_PATH=../lib/gnat
endif

build: $(SUPPORTLIB) aunit_tests.gpr
	$(PROJECT_PATH_ARG) $(GPRBUILD) -p -Paunit_tests $(CONF_ARGS) -XRUNTIME=$(RTS) -XPLATFORM=$(TARGET) $(VARIANT) $(LARGS)

run: build
	-$(RUN) ./exe/$(TARGET)-$(RTS)/aunit_harness$(exeext)

test: build
	-$(RUN) ./exe/$(TARGET)-$(RTS)/aunit_harness$(exeext) > test.out.full 2>&1
	egrep "^Total|^Success|^Fail|^Unexp" test.out.full > test.out
	diff -b test.out expected.out
	@echo
	@echo "[OK] AUNIT TEST IS SUCCESSFUL"

aunit_tests.gpr: aunit_tests.gpr.in force
	rm -f $@
	touch $@
ifneq ($(strip $(filter-out full pthread xenomail kernel default, $(RTS))),)
ifeq ($(NO_ZFP_SUPPORT),)
	echo "with \"zfp_support\";" >> $@
endif
endif
	@cat $< >> $@

clean:
	$(RM) -rf obj exe support/obj support/lib *.cgpr test.out

RMDIR	= rmdir
MKDIR	= mkdir -p
RM	= rm
CP	= cp -p
