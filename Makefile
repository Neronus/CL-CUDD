
CUDD = cudd-3.0.0

all: $(CUDD)/Makefile
	+$(MAKE) -C $(CUDD) all-am

$(CUDD)/Makefile: $(CUDD)
	cd $(CUDD) ; ./configure --enable-shared --disable-static

$(CUDD):
	curl ftp://vlsi.colorado.edu/pub/$(CUDD).tar.gz | tar zx
