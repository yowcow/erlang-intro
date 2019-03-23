.PHONY: test eunit hello clean

REBAR := rebar3

all:
	$(REBAR) compile

test: eunit

eunit:
	$(REBAR) eunit

hello:
	erl -noshell -s hello_world start -s init stop

clean:
	-rm -rf $(REBAR) _build *.beam
