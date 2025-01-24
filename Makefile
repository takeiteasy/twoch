APPNAME:=twoch

.PHONY: default build rebuild veryclean clean run rerun stop start

default: clean run

build: Dockerfile
	docker build --no-cache -t $(APPNAME) . \
		--build-arg SBCL_VERSION=$(shell cat VERSION-SBCL.txt) \
		--build-arg QUICKLISP_PKGS="$(shell tr '\n' ' ' < quicklisp-libraries.txt)"

rebuild: veryclean build run

veryclean: clean
	docker image rm $(APPNAME)

stop:
	docker stop $(APPNAME)

start:
	docker start $(APPNAME)

clean: stop
	docker rm $(APPNAME)

run:
	docker run -d \
			--mount type=bind,source=$(PWD),target=/src \
			--name $(APPNAME) \
			--publish 2345:5000 \
	$(APPNAME) sbcl --eval '(ql:quickload "asdf")' \
					--eval '(asdf:load-system "$(APPNAME)")' \
					--eval '(loop)'

rerun: clean run
