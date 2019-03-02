(stack build && stack exec hed) 2>&1 | tee out
stty sane
#(stack build --library-profiling --executable-profiling --ghc-options "$optflags -rtsopts -Werror -ferror-spans -fprof-auto -fprof-cafs" && \
# stack exec) 2>&1 | tee out
