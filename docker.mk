all:
	docker pull erlang

cmd/%:
	docker run \
		--rm -it \
		-v `pwd`:/app \
		--workdir /app \
		erlang sh -c 'make $*'

.PHONY: all
