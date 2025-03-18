todo:
    delta \
        <(basename -as.c exstrom/*.c) \
        <(dirname app/*/Main.hs | xargs basename -a)
