REBAR=rebar3

all: compile

compile:
	$(REBAR) compile

check_all: test lint

test: xref dialyzer eunit

dialyzer:
	$(REBAR) dialyzer

xref:
	$(REBAR) xref

eunit:
	$(REBAR) eunit

lint:
	$(REBAR) as lint lint

clean:
	$(REBAR) clean -a

nuke:
	rm -rf ./_build/

.PHONY: all compile check_all test dialyzer xref eunit lint clean nuke
