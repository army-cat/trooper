REBAR ?= $(shell which rebar3 2>/dev/null || echo ./rebar3)

all: compile test

doc:
	$(REBAR) ex_doc

clean-devel: clean
	-rm -rf _build

clean:
	-rm -f .build_date
	$(REBAR) clean

compile:
	$(REBAR) compile

test:
	$(REBAR) do xref, dialyzer, eunit, cover, covertool generate
	cp _build/test/covertool/trooper.covertool.xml cobertura.xml

shell:
	$(REBAR) as dev shell

.PHONY: doc test compile all shell
