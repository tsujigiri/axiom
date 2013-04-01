PROJECT = axiom
DIALYZER = dialyzer
REBAR = rebar

all: app

deps:
	$(REBAR) get-deps

app: deps
	$(REBAR) compile

clean:
	$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump

doc: clean-doc
	$(REBAR) doc skip_deps=true

clean-doc:
	rm -f doc/*.css
	rm -f doc/*.html
	rm -f doc/*.png
	rm -f doc/edoc-info

test: ct dialyze doc

test-build:
	$(REBAR) -C rebar.test.config -DAXIOM_TEST compile

ct: clean test-build
	$(REBAR) -C rebar.test.config ct skip_deps=true

build-plt:
	$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps kernel stdlib sasl inets crypto public_key ssl \
		./deps/ranch/ebin ./deps/cowboy/ebin ./deps/erlydtl/ebin

dialyze: clean deps test-build
	$(DIALYZER) --plt .$(PROJECT).plt ebin

ci: ct
