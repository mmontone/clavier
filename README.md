Clavier
----------

*Clavier* is a general purpose validation library for Common Lisp.

Documentation
-------------

[HTML documentation](http://mmontone.github.io/clavier/doc/build/html/index.html)

[PDF manual](http://mmontone.github.io/clavier/doc/build/latex/validators.pdf)

Install
-------

Download the source code from https://github.com/mmontone/clavier and point `.asd` system definition files from `./sbcl/system (ln -s <system definition file path>)` and then evaluate:

```lisp
(require :clavier)
```

from your lisp listener. 

You will also need to satisfy these system dependencies:

- `alexandria`
- `cl-json`
- `cl-ppcre`
- `parenscript`
- `closer-mop`

The easiest way of installing those packages is via [Quicklisp](http://www.quicklisp.org/).

This library is under the MIT licence.

Getting started
---------------
