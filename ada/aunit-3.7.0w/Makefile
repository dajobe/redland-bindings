RTS =
TARGET =
GPRBUILD  = gprbuild
GPRCLEAN = gprclean

INSTALL:=$(shell exec=`which gprbuild`;if [ ! -x "$$exec" ]; then unset exec;fi;echo $$exec | sed -e 's/\/bin\/$(GPRBUILD).*//')

ifeq ($(RTS),)
   RTS=full
   RTS_CONF =
else
   RTS_CONF = --RTS=$(RTS)
endif

ifeq ($(TARGET),)
   TARGET=native
   TARGET_CONF =
else
   TARGET_CONF = --target=$(TARGET)
endif

MODE = Install

CONF_ARGS = $(TARGET_CONF) $(RTS_CONF)

# Install directories

I_INC   = $(INSTALL)/include/aunit
I_LIB   = $(INSTALL)/lib/aunit
I_GPR   = $(INSTALL)/lib/gnat
I_TPL   = $(INSTALL)/share/examples/aunit
I_DOC   = $(INSTALL)/share/doc/aunit
I_PLG   = $(INSTALL)/share/gps/plug-ins

.PHONY: all clean targets install_clean install

all:
	$(GPRBUILD) -Plib/gnat/aunit_build -p -XMODE=$(MODE) -XRUNTIME=$(RTS) -XPLATFORM=$(TARGET) $(CONF_ARGS)

clean:
	$(RM) -fr lib/aunit lib/aunit-obj
	-${MAKE} -C docs clean

install_clean:
ifeq ($(INSTALL),)
	@echo 'Error when installing: $$INSTALL is empty...'
	@echo "Please set an installation path before installing !"
else
	$(RM) -rf $(I_DOC)
	$(RM) -rf $(I_TPL)
	$(RM) -f $(I_PLG)/aunit.xml
	$(RM) -rf $(I_LIB)
	$(RM) -rf $(I_INC)
	$(RM) -f $(I_GPR)/aunit_build.gpr
	$(RM) -f $(I_GPR)/aunit.gpr
	$(RM) -f $(I_GPR)/aunit_shared.gpr
endif

install: install_clean
ifneq ($(INSTALL),)
	$(MKDIR) $(I_DOC)
	$(MKDIR) $(I_TPL)
	$(MKDIR) $(I_PLG)
	$(MKDIR) $(I_GPR)
	$(MKDIR) $(I_LIB)
	$(MKDIR) $(I_INC)
	@for doc in docs/*.html docs/*.info docs/*.pdf docs/*.txt ; do \
	   if [ -f $$doc ]; then \
	      echo $(CP) $$doc $(I_DOC) ; \
	      $(CP) $$doc $(I_DOC) ; \
           fi \
        done
	-$(CP) support/aunit.xml $(I_PLG)

	-$(CPR) lib/gnat lib/aunit $(INSTALL)/lib
	-$(CPR) include/aunit $(INSTALL)/include
	-$(CPR) share/examples $(INSTALL)/share
	@echo '------------------------------------------------------------------'
	@echo '--  AUnit has now been installed.'
	@echo '--  To be able to use the library, you may need to update your'
	@echo '--  ADA_PROJECT_PATH or GPR_PROJECT_PATH to point to the path'
	@echo '--  $(I_GPR)'
	@echo '------------------------------------------------------------------'
endif

doc:
	${MAKE} -C docs

RMDIR	= rmdir
MKDIR	= mkdir -p
RM	= rm
CP	= cp
CPR	= cp -r
CHMOD	= chmod
