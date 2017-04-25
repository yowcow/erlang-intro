.PHONY: test eunit clean

REBAR := ./rebar3

all: $(REBAR)
	$(REBAR) compile

$(REBAR):
	curl -LO https://s3.amazonaws.com/rebar3/rebar3
	chmod +x rebar3

test: eunit

eunit:
	$(REBAR) eunit

clean:
	-rm -rf $(REBAR) _build *.beam
