# MiniJavaCompiler

## Setup

To build the Main.native or Main.byte, stay in current folder
```
$ ocamlbuild Main.byte
```  
or  
```
$ ocamlbuild Main.native
```
## Run

To execute the program, stay in current folder 
```
$ ./Main.byte [file_to_be_analyse.java]
```

```
$ ./Main.native [file_to_be_analyse.java]
```

Example  
```
$ ./Main.byte test.java
```

## Tips from professor

'ocamlbuild Main.byte' (or native) to build the compiler. The main file
is Main/Main.ml, it should not be modified. It opens the given file,
creates a lexing buffer, initializes the location and call the compile
function of the module Main/compile.ml. It is this function that you
should modify to call your parser.

'ocamlbuild Main.byte -- <filename>' (or native) to build and then execute
the compiler on the file given. By default, the program searches for
file with the extension .java and append it to the given filename if
it does not end with it.

If you want to reuse an existing ocaml library. Start by installing it
with opam. For example, to use colored terminal output you
use 'opam install ANSITerminal'.
Then you must inform ocamlbuild to use the ocamlfind tool :
'ocamlbuild -use-ocamlfind Main.byte -- tests/UnFichierDeTest.java'
et vous devez ajouter au fichier _tags la bibliothèque en question par exemple :
true: package(ANSITerminal)


## Branch

- master: branch to deliver
- expr:   expr group's work  (please merge to this branch only when it works)
- expr-dev : expr dev branch (do whatever you like in this branch)