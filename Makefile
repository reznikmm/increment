all:
	cd testsuite;\
	uaflex --types Types --scanner Tests.Lexers \
	 --handler Handlers --tokens Nodes.Tokens test.uaflex; \
	rm types.ads handlers.ads tests-lexers-on_accept.adb
	gprbuild -P gnat/increment_tests.gpr