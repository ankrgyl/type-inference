About
=====

For Foundations of Programming Languages (15-312) at Carnegie Mellon University, we had to write a
polymorphic type inferring compiler for a small ML dialect (MinML).

The compiler uses generalization and specialization with let-polymorphism to perform the type inference.
These techniques are explained in hw04.pdf.

Running It
==========

You must have [SML/NJ](http://www.smlnj.org/) installed on your machine. I would recommend installing rlwrap as well, because
without it the SML interpreter is pretty unwieldy. Note that the `rlwrap` below is optional.

From the root of the project:

```
$ rlwrap sml
Standard ML of New Jersey v110.72 [built: Sun Jan 30 15:13:54 2011]
- CM.make "sources.cm";
- Test.runAll();
```

To run a file and launch a MinML REPL (you need to define variables in the file), use

```
- TopLevel.replFile "filename"
```

For example, from the project root you can do

```
- TopLevel.replFile "tests/simple.mml"
val lol = fn : 'b -> 'a -> 'b
-> 
```
