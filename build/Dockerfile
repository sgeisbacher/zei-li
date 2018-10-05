FROM haskell
RUN mkdir /project &&\
    cabal update &&\
    cabal install aeson MissingH http-conduit
WORKDIR /project
