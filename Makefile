APPNAME = twoch

.PHONY: default build cleanbuild veryclean clean run cleanrun stop start

default: clean run

build: Dockerfile
	docker build --no-cache -t $(APPNAME) . \
		--build-arg PKGS="$(shell tr '\n' ' ' < quicklisp.txt)"

cleanbuild: veryclean build run

veryclean: clean
	docker image rm $(APPNAME)

stop:
	docker stop $(APPNAME)

start:
	docker start $(APPNAME)

clean: stop
	docker rm $(APPNAME)

log:
	docker logs $(shell docker container ls --all | grep -n "" | grep "^2:" | cut -d: -f2- | cut -d' ' -f1)

run:
	docker run -d \
			--mount type=bind,source=$(PWD),target=/$(APPNAME) \
			--name $(APPNAME) \
			--publish 2345:5000 \
	$(APPNAME) sbcl --eval '(ql:quickload "asdf")' \
					--eval '(asdf:load-system "$(APPNAME)")' \
					--eval '(loop)'

cleanrun: clean run
