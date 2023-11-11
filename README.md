# Functional Programming Lab: Write Your Own Compiler/Interpreter

The goal of this lab is to invent your own functional programming language and develop an interpreter/compiler for it.

You may do this lab in the group of 2 or 3 people (or more - but this requires approval from the lecturer):

* 2 people - compiler/interpreter + sample programs + short documentation in README.md (feel free to replace this file with your own documentation)
* 3 people - compiler/interpreter + sample programs + more detailed documentation in GitHub Pages
* 3+ people - in addition to above, this may include some of the following:
  - in-browser IDE
  - Jupyter Notebook support
  - Translation to JavaScript, so that you can execute the program in browser
  - VS Code Extensions

## Task

Your goal would be to invent and implement your own functional programming language. The requirements are:

* It has to closely follow functional programming paradigm, based on either [Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) or [Combinatory Logic](https://en.wikipedia.org/wiki/Combinatory_logic).
* It has to be more or less Turing-complete, i.e. implement recursion
* You should be able to program and run a function to calculate factorial.

> Keep in mind that writing parsers is a tedious task, so try to keep the syntax of the language as simple as possible. 

For inspiration:

* Look into [LISP](https://books.ifmo.ru/file/pdf/1918.pdf) - a programming language with very simple syntax
* Combinatory parsers and [fparsec](https://www.quanttec.com/fparsec/) library, if you want to implement a language with more complex syntax
* [Top-Down Parser in F#](https://github.com/fholm/Vaughan)
* Interesting blog post on [Parsing in F#](https://www.erikschierboom.com/2016/12/10/parsing-text-in-fsharp/)
* Parsing using [FsLex and FsYacc tooling](https://realfiction.net/posts/lexing-and-parsing-in-f/) (not recommended)
* Implementation of [Scheme in F#](https://github.com/AshleyF/FScheme) - you may look at this project for inspiration, but do not borrow code from there!

## Evaluation Criteria

* Turing Completeness
* Program Samples, including, but not limited to factorial
* Originality and beauty of syntax
* Documentation
* Beauty of implementation

Preferred language for implementation is F#.

In the documentation, please mention explicitly which language features you have implemented:

* [ ] Named variables (`let`)
* [ ] Recursion
* [ ] Lazy evaluation
* [ ] Functions
* [ ] Closures
* [ ] Library functions: File IO
* [ ] Lists / Sequences
* [ ] Library functions: Lists/Sequences

## Repository

You need to work on the code in GitHub Classroom repo. After you finish the task, it is **highly recommended** to fork this code in your own GitHub accounts, so that is can act as your portfolio.

## Working Step-By-Step

Because the projects is rather big, you need to do it in several stages, checking in your code to GitHub in between:

* Stage 1: Developing abstract syntax tree and parser for your language + one sample program
* Stage 2: Developing interpreter/compiler for your language
* Stage 3: Writing sample programs and documentation

> Of course, you are welcome to modify the language at later stages as you see fit.

## Credits

Do not forget to mention your team in the README.md file, specifying also who did what. Also, README.md should include a short tutorial on your language, and some short code samples.

Name | Role in the project
------------------|---------------------
... | ...

<img src="https://soshnikov.com/images/byhuman_en.png" height="25px"/>

> If you use Generative AI when writing this code (ChatGPT, GitHub Copilot and such), you need to mention it here (and remove the banner above), and briefly describe how it was used, and how it made you more productive. *Using Generative AI without explicitly mentioning it is a violation of academic conduct!* 

