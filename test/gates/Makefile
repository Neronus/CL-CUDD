
format = pdf
all: $(addsuffix .$(format),$(basename $(wildcard *.dot)))

%.$(format): %.dot
	dot $< -Tpdf -o $@

clean:
	-rm *dot* *.$(format) *.pdf
