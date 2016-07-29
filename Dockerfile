FROM haskell:7
MAINTAINER Alex Brandt <alunduil@alunduil.com>

EXPOSE 8000
WORKDIR /usr/local/src/murl

RUN cabal update

COPY ./murl.cabal /usr/local/src/murl/murl.cabal
RUN cabal install --only-dependencies

COPY . /usr/local/src/murl
RUN cabal install 

ENTRYPOINT [ "murl" ]
