# ProgramAnalysis02242

## Open Individual Tasks:

### Integration Testing Setup
Assignee: Lasse
- Each test case is represented by a folder
- The "test.c" file contains the program to be tested
- All other files in the folder contains a specific type of test to check
  - The first line contains the command line parameters to run for the test
  - The other lines contains the expected output of the analysis program

### Program Graph Visualization
Assignee:
- A module which translates the program graph into GraphViz syntax
- Nice if it automatically calls "dot" and converts the GraphViz output into a png

### Control Flow analysis
Assignee: Thomas
- Implement the edges functions transforming the AST into the program graph

### Local Scope / Resolution Pass
Assignee: Lasse

### Compiler Driver
Assignee: Lasse
- A main entry point which in a flexible manner can drive the parsing and analysis process

### Reaching Definitions
Assignee: Nicolai
- Implement worklist


### Expression Pretty Printer
Assignee: Thomas
- Lasse expects minimal but sufficient number of parenthesis


## Future Tasks:

### Proper Error Handling
- Requires extensions to the AST and ProgramGraph. It is therefore best postponed until the stabilization of the control flow analysis.
