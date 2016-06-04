
CUDD = cudd-3.0.0

all: $(CUDD)/cudd/.libs/libcudd.so

$(CUDD)/cudd/.libs/libcudd.so:
	curl ftp://vlsi.colorado.edu/pub/$(CUDD).tar.gz | tar zx
	cd $(CUDD) ; ./configure --enable-shared --disable-static
	+$(MAKE) -C $(CUDD) all-am


