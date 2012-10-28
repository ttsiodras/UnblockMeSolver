
For working with the code in the REPL,

$ ocaml
# #load "bigarray.cma";;

# #use "Unblock.ml";;
...

...but first make sure the '_' function is renamed to something else.

In Vim:

    if you build with -annot, Vim's OCaml plugin can show you
    the types of symbols, via LocalLeader-t (i.e. \ t )
    it can also comment and uncomment areas via LocalLeader-c/C
