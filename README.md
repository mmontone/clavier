Clavier
----------

[![Quicklisp](http://quickdocs.org/badge/clavier.svg)](http://quickdocs.org/clavier/)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

*Clavier* is a general purpose validation library for Common Lisp.

Install
-------

Through Quicklisp:

```lisp
(ql:quickload :clavier)
```
Getting started
---------------

Validators are class instances that validate the arguments passed to the `validate` function:

```lisp
(let ((validator (make-instance 'equal-to-validator :object 22)))
    (validate validator 22 :error-p t))
    
;=> T
```

If the validator succeeds, the validate function returns `T`. If it fails, the validate function either signals a validation error, or returns `NIL` and a validation message depending on the value of the `:error-p` argument.

```lisp
(let ((validator (make-instance 'equal-to-validator :object 22)))
    (validate validator 33 :error-p nil))
    
;=>
;NIL    
;"33 is not equal to 22"
```

Validators are implemented as funcallable classes. So, alternatively to using the `validate` function, it is possible to just funcall the validator, like this:

```lisp
(let ((validator (make-instance 'equal-to-validator :object 22)))
    (funcall validator 22 :error-p t))
    
;=> T
```

## Validation expressions

It is possible to create validators with a more convenient syntax. Each validator provides a builder function. For instance, and equal-to-validator can be built like this:

```lisp
(funcall (== 100) 100) ;=> T
(funcall (== 100) 99) ;=> NIL
```

## Validators composition

This allows to compose validators, using `==`, `~=`, `&&`, `||` as the composition operands:

```lisp
(let ((validator (|| (&& (greater-than 20)
			   (less-than 30))
		       (|| (&& (greater-than 1)
			       (less-than 10))
			   (== 100)))))
    (funcall validator 5))
```

For example, this is how to accept a blank object, but validate it if it isn't blank:


~~~lisp
(defparameter *validator* (clavier:||
                                   (clavier:blank)
                                   (clavier:&& (clavier:is-a-string)
                                               (clavier:len :min 10)))
  "Allow a blank value. When non blank, validate.")

(funcall *validator* "")
;; =>
T
NIL

(funcall *validator* "asdfasdfasdf")
;; =>
T
NIL

(funcall *validator* "asdf")
;; =>
NIL
"Length of \"asdf\" is less than 10"

(funcall *validator* 2)
;; =>
NIL
"2 is not a string"
~~~


## Validators messages

Validators messages to be used when validation fails can be customized passing an `:message` argument when building the validator

## Catching and collecting validation errors

Validation errors can be controlled globally by setting the dynamic variable `*signal-validation-errors*`, which is `NIL` by default (no validation errors are signaled by default).

There's also the `with-signal-validation-errors` macro to specify whether validation errors should be signaled or not in a dynamic extent. For instance, this code signals a validation error:

```lisp
(let ((validator (make-instance 'equal-to-validator :object 22)))
	   (with-signal-validation-errors (t)
	     (validate validator 33)))
```

Use the `collecting-validation-errors` macro to collect validation errors happening in a dynamic extent:

```lisp
(let ((validator (make-instance 'equal-to-validator :object 22)))
	   (collecting-validation-errors (errors found-p)
	       (progn
		 (funcall validator 33 :error-p t)
		 (funcall validator 44 :error-p t))
	     (print errors)
	     (print found-p)))
;=>
;(#<VALIDATION-ERROR 44: 44 is not equal to 22 {1008A48673}>
; #<VALIDATION-ERROR 33: 33 is not equal to 22 {1008A47EA3}>) 
;T 
```

## Validators list:

This is the list of available validator classes and their shortcut function:

* equal-to-validator `(==)`
* not-equal-to-validator `(~=)`
* blank-validator `(blank)`
* not-blank-validator `(not-blank)`
* true-validator `(is-true)`
* false-validator `(is-false)`
* type-validator `(is-a type)`
* string-validator `(is-a-string)`
* boolean-validator `(is-a-boolean)`
* integer-validator `(is-an-integer)`
* symbol-validator `(is-a-symbol)`
* keyword-validator `(is-a-keyword)`
* list-validator `(is-a-list)`
* function-validator `(fn function message)`
* email-validator `(valid-email)`
* regex-validator `(matches-regex)`
* url-validator `(valid-url)`
* datetime-validator `(valid-datetime)`
* pathname-validator `(valid-pathname)`
* not-validator `(~ validator)`
* and-validator `(&& validator1 validator2)`
* or-validator `(|| validator1 validator2)`
* one-of-validator `(one-of options)`
* less-than-validator `(less-than number)`
* greater-than-validator `(greater-than number)`
* length-validator `(len)`
