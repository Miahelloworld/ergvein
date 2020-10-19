# Development build of GHCJS version of frontend
cabal new-build exe:ergvein-faucet-front
cp dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/ergvein-faucet-front-1.0.0.0/x/ergvein-faucet-front/build/ergvein-faucet-front/ergvein-faucet-front.jsexe/all.js ./statics/main.js
