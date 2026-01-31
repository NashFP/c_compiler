# Writing a C Compiler

Nora Sandler's No Starch Press book
["Writing a C Compiler"](https://nostarch.com/writing-c-compiler)
may be an interesting way to explore writing a compiler
in a functional language.

There is some initial setup involved. The book includes a suite
of tests here: [https://github.com/nlsandler/writing-a-c-compiler-tests/](https://github.com/nlsandler/writing-a-c-compiler-tests/)

In order to use these tests, you need a `driver.sh` script that will
run the C preprocessor on your file and then potentially run the assembler
and linker on your code. There is a stub `driver.sh` in this directory
that you can modify to execute your compiler. This stub assumes it
is running on Linux.

The book mentions running on Apple Silicon. It suggests this command:
```
arch -x86_64
```

This mostly works, except that the Mac uses clang instead of gcc, and
the assembly file requires a few changes. I modified the default
`driver.sh` to check to see if it is running on a Mac, and if it is,
it does a very minimal fixup. I don't know if this will work for
everyone in the long run, but on my Macbook Air, I am able to pass
all the tests at least through chapter 3 with the minimal changes.

The book has a companion web site here: [https://norasandler.com/book/](https://norasandler.com/book/) and it has links to many useful tools including
the Intel 64 Software Developer's Manual and Matt Godbolt's [Compiler Explorer](https://godbolt.org/)
