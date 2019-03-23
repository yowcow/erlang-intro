DOCKER_RUN_OPT := --rm -it -v `pwd`:/app --workdir /app

all:
	docker pull erlang

erl:
	docker run $(DOCKER_RUN_OPT) erlang

cmd/%:
	docker run $(DOCKER_RUN_OPT) erlang sh -c 'make $*'

.PHONY: all
