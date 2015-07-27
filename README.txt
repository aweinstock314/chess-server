Server for chess games. Project for HackUMass Spring 2015.
-----
Instructions for building:
cabal sandbox init && cabal install --only-dependencies && cabal configure && cabal build
CHESS_SERVER_PORT=8000 ./dist/build/chess-server/chess-server
