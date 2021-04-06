# Program Analysis

This repository contains a tool for performing static analysis of programs written in the toy *MicroC* programming language first described in "Formal Methods - An Appetizer" by Flemming Nielson and Hanne Riis Nielson (2019). The tool is capable of a wide range of distinct control-flow and data-flow analyses implemented using an elegant monotone framework approach.
Furthermore, the tool supports several distinct Worklist based algorithms and a benchmark suite for testing their performance.

## Installation

Requires a correctly setup installation of .NET Core 3.1 and the F# programming language. Building the project can then simply be done as follows:
```
dotnet restore
dotnet build --configuration Release --no-restore
```
The test suite can be run using ```dotnet test```


## Architecture 

### Front-end

The analysis tool is implemented using the F# programming language. A custom lexer and parser is used to convert the source code into an AST: The lexer generates a stream of tokens which is then parsed using a recursive descent Pratt parser (Pratt, 1973) which allows precedence of infix operators to be nicely handled. A name resolution pass is used to bind identifiers to their matching declarations using a stack to correctly handle nested scopes. The name resolution also allows determining whether any memory locations are used before their declaration.

### Control-flow analysis

The tool uses Program Graphs (Nielson and Nielson, 2020) as a output for the control-flow analysis and as a basis data structure for all subsequent data-flow analyses. In brief, Program Graphs are closely related to standard flow graphs but switches around the graph-based representation such that program states are represented by vertices and program actions are represented by directed edges. These two representations are equally powerfull and it is trivial to transform between them. Nonetheless, program graphs have the advantage of being easier to grasp for many with a background in formal languages due to their similarities with automata.

### Data-flow analyses

The tool supports 8 data-flow analyses, all implemented using a general monotone framework approach (Nielson, Nielson and Hankin, 2005). The supported analyses are:

#### Reaching Definitions
Determines for each program point, which variable definitions reach said program paint, e.g., 'x := a' reaches a point *p* if along some path of execution to the program point *p*, *x* may still be defined by the this assignment. Useful for finding the links between where variables are modified and where their values are used.
#### Live Variables
Determines for each program point, which memory locations are *live* at the exit of the point, i.e., which memory locations with their current assignment could potentially be used in further computation along some path after the given point. Useful for register allocation and automated memory management.
#### Available Expressions
Determines for each program point, which expressions must have already been computed and not later modified on all paths to this point. Useful for Common Subexpression Elimination.
#### Very-Busy Expressions
An expression is said to be *very busy* if it for all paths, that may be taken from a given program ponit, always must computed before any of its variables are redefined. Useful for code hoisting.
#### Dangerous Variables
A more advanced version of the Reaching Definitions analysis useful for finding uninitialized variables. A variable is marked as *dangerous* at a given program point if it 1) it has not yet been assigned any value or 2) is the result of a computation which involves at least one variable already marked as *dangerous* at this program point.
#### Faint Variables
A more advanced version of the Live Variables analysis, which takes into account that a variable, that will later be used for some computation, might still be dead if the result of that computation is also dead. Thus the faint variables analysis produces a more accurate result than the live variables analysis.
#### Detection of Sign
Determines the potential signs of every integer variable at every program point. Can be used for a wide varity of sanity checks such as avoding certain division-by-zero or array access at a negative index.
#### Intervals
Determines the lower and upper bound of the values stored in each memory location at every program point. Useful for checking for integer overflows or static checks for possible array out-of-bounds access.

## Bibliography

- Nielson, Flemming and Nielson, Hanne Riis (2020). *Program Analysis - An Appetizer*.
- Nielson, Flemming and Nielson, Hanne Riis (2019). *Formal methods - An Appetizer*.
- Nielson, Flemming and Nielson, Hanne Riis, Hankin Chris (2005). *Principles of program analysis*.
- Pratt, Vaughan (1973). *Top down operator precedence*
