GPRBUILD_FLAGS = -p -j0
PREFIX                 ?= /usr
GPRDIR                 ?= $(PREFIX)/share/gpr
LIBDIR                 ?= $(PREFIX)/lib
INSTALL_PROJECT_DIR    ?= $(DESTDIR)$(GPRDIR)
INSTALL_INCLUDE_DIR    ?= $(DESTDIR)$(PREFIX)/include/increment
INSTALL_LIBRARY_DIR    ?= $(DESTDIR)$(LIBDIR)
INSTALL_ALI_DIR        ?= ${INSTALL_LIBRARY_DIR}/increment

GPRINSTALL_FLAGS = --prefix=$(PREFIX) --sources-subdir=$(INSTALL_INCLUDE_DIR)\
 --lib-subdir=$(INSTALL_ALI_DIR) --project-subdir=$(INSTALL_PROJECT_DIR)\
--link-lib-subdir=$(INSTALL_LIBRARY_DIR)

all: source/generated/tests-lexers-tables.adb source/generated/incr-ada_lexers-tables.adb
	gprbuild -p -P gnat/increment.gpr
	gprbuild -p -P gnat/increment_tests.gpr

source/generated/tests-lexers-tables.adb: testsuite/test.uaflex
	cd source/generated;\
	uaflex --types Types --scanner Tests.Lexers \
	 --handler Handlers --tokens Nodes.Tokens ../../testsuite/test.uaflex;\
	rm types.ads handlers.ads tests-lexers-on_accept.adb

source/generated/incr-ada_lexers-tables.adb: source/ada/ada.uaflex
	cd source/generated;\
	uaflex --types Types --scanner Incr.Ada_Lexers \
	 --handler Handlers --tokens Nodes.Tokens ../ada/ada.uaflex; \
	rm types.ads handlers.ads incr-ada_lexers-on_accept.adb

install:
	gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/increment.gpr

check: all
	.objs/tests-driver testsuite/test_01.xml
	.objs/tests-driver testsuite/test_02.xml
	.objs/tests-driver testsuite/test_03.xml
	.objs/tests-driver testsuite/test_04.xml
	.objs/tests-driver testsuite/test_05.xml
	.objs/tests-driver testsuite/test_06.xml
	@echo Tests OK!
