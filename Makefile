REBAR := rebar3

all:
	$(REBAR) compile

test: eunit

eunit:
	$(REBAR) eunit

hello:
	erl -noshell -s hello_world start -s init stop

clean:
	$(REBAR) clean

realclean: clean
	-rm -rf _build *.beam

.PHONY: all test eunit hello clean realclean
