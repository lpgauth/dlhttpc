PROJECT=dlhttpc
REBAR=./rebar

all: compile doc

build-plt:
	dialyzer --build_plt --output_plt ~/.$(PROJECT).plt \
		--apps erts kernel stdlib crypto public_key ssl deps/*/ebin

check-plt:
	dialyzer --check_plt --plt ~/.$(PROJECT).plt

compile:
	$(REBAR) get-deps compile

doc:
	$(REBAR) doc skip_deps=true

test:
	$(REBAR) eunit skip_deps=true

dialyze:
	dialyzer ebin/*.beam --plt ~/.$(PROJECT).plt -I include

release: all dialyzer test
	$(REBAR) release

clean:
	$(REBAR) clean

.PHONY: all doc clean test dialyzer
