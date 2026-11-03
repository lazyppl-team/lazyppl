if [ -z "$LAZYPPL_GREETED" ]; then
    export LAZYPPL_GREETED=1
    echo 'Welcome to LazyPPL. Try `cabal run -fdemos regression-exe` or `cabal run -fdemos wiener-exe`. Plots land in the workspace.'
fi
