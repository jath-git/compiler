# Compiler for Subset of C++

### Download Racket
- Install from https://download.racket-lang.org, then add racket to $PATH OR
- Use package management system to install from shell

### Run Command
- Run "racket compiler.rkt [one of <scan, parse, analyze, assembly, binary>] [optional int] [optional int] < sample.txt"
- If sample.txt follows the template of program below, compiles
- Otherwise, throws an error with compilation message

## Template of Program

[optional: <procedure(s) with body format like main>]

int main(int <id name>, int <id name>) {
  [optional <int declaration or int* declaration set to NULL>]

  [optional <statements (e.g. arithmetic, if, while)>]

  return <int expression>
}