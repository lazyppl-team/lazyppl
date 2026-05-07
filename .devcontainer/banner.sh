if [ -z "$LAZYPPL_GREETED" ]; then
    export LAZYPPL_GREETED=1
    echo 'Welcome to LazyPPL. Try `stack run regression-exe` or `stack run wiener-exe`. Plots land in the workspace.'
fi
