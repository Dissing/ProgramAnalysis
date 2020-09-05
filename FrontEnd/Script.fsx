// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library.fs"
open FrontEnd

let example1 = """
read hello;
x := x + y - z * 2;
if (x > y) {
  write goodbye;
} else {
  while (true) {
    A[-1] := B[3];
  }
}
"""

printfn "%A" (Lexer.Lex example1)

// Define your library scripting code here

