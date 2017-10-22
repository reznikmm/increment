all: source/generated/tests-lexers-tables.adb source/generated/incr-ada_lexers-tables.adb
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
