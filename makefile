# Get rid of the annyong tabs required by default
.RECIPEPREFIX = >

# Compile p2p_client to native code
all: compile_and_link clear_intermediate_files

# Compile and link
compile_and_link:
> ocamlfind ocamlopt -thread -syntax camlp4o \
> -package lwt,lwt.syntax,lwt.unix,cohttp.lwt,uri,str,markup \
> -linkpkg -o PullPlayerData types.ml utils.ml draft_kings.ml main.ml

# Make a bytecode executable
compile_and_link_byte:
> ocamlfind ocamlc -custom -thread -syntax camlp4o \
> -package lwt,lwt.syntax,lwt.unix,cohttp.lwt,uri,str,markup \
> -linkpkg -o PullPlayerData types.ml utils.ml draft_kings.ml main.ml

clear_intermediate_files:
> rm *.cmi *.cmx *.o

clear_intermediate_files_byte:
> rm *.cmi *.cmo
