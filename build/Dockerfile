FROM haskell
RUN mkdir /project &&\
    cabal update &&\
    cabal install aeson MissingH
WORKDIR /project
