if [ -z "$@" ]
then
  ARGS=""
else
  ARGS="\"$@\""
fi
ghcid --colour=always -c "cabal new-repl $ARGS"
