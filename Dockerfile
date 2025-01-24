FROM alpine:latest

ARG SBCL_VERSION
ARG QUICKLISP_PKGS

RUN apk update && apk upgrade
RUN apk add sbcl=$SBCL_VERSION curl
RUN curl -o /tmp/quicklisp.lisp 'https://beta.quicklisp.org/quicklisp.lisp'
RUN sbcl --noinform --non-interactive --load /tmp/quicklisp.lisp --eval \
        "(quicklisp-quickstart:install)"
RUN sbcl --noinform --non-interactive --load ~/quicklisp/setup.lisp --eval \
        '(ql-util:without-prompting (ql:add-to-init-file))'
RUN rm -r /tmp/quicklisp.lisp
RUN echo '#+quicklisp(push "/src" ql:*local-project-directories*)' >> ~/.sbclrc
ADD . /src
WORKDIR /src
RUN sbcl --noinform --non-interactive --eval "(mapc #'ql:quickload '($QUICKLISP_PKGS))" --quit
