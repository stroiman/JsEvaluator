JsEvaluator
===========

This project consist of the source code that I used for a presentation about
the use of FsLex and FsYacc for creating parsers in F#.

The solution is a small parser for a small language with a JavaScript like
syntax. But this is no where near a complete parser. It is only meant to
demonstrate some of the principles of parsing input, generating syntax trees,
and consuming these in an evaluator.

### Building code ###

I have used the `paket` package manager. To get started run the following

    paket.bootstrapper.exe
    paket install

The bootstrapper downloads the actual package manager, and the install command
installs the packages. Once packages are installed, you can build from Visual
Studio, or from the command line with

    msbuild build.proj

### Using `identifier` rules for keywords

During my talk, I mentioned that the generated lexer could be simplified by
introducing a map of keywords, and using the 'identifer' rule to find keywords,
instead of having explicit rules for each keyword.

This can change can be seen by comparing the tags
`before-simplifying-tokenizer` and `after-simplifying-tokenizer`
