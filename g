#stack build --ghc-options='-fno-ignore-asserts -threaded' && stack exec hed
(stack build --ghc-options='-fno-ignore-asserts -threaded' 2>&1 | tee out) && stack exec hed
stty sane
#(stack build --library-profiling --executable-profiling --ghc-options "$optflags -rtsopts -Werror -ferror-spans -fprof-auto -fprof-cafs" && \
# stack exec) 2>&1 | tee out
