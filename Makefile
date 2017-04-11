.PHONY: eunit clean

all: rebar3
	./rebar3 compile

rebar3:
	curl -LO https://s3.amazonaws.com/rebar3/rebar3
	chmod +x rebar3

eunit:
	./rebar3 eunit

clean:
	-rm -rf _build
