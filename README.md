Clavier
----------

*Clavier* is a general purpose validation library for Common Lisp.

Install
-------

Download the source code from https://github.com/mmontone/clavier and point `.asd` system definition files from `./sbcl/system (ln -s <system definition file path>)` and then evaluate:

```lisp
(require :clavier)
```
from your lisp listener. 

You will also need to satisfy these system dependencies:

- `alexandria`
- `cl-ppcre`
- `closer-mop`

The easiest way of installing those packages is via [Quicklisp](http://www.quicklisp.org/).

This library is under the MIT licence.

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

## Validators messages

Validators messages to be used when validation fails can be customized passing an `:message` argument when building the validator

## Catching and collecting validation errors

Validation errors can controlled globally by setting the dynamic variable `*signal-validation-errors*`, which is `NIL` by default (no validation errors are signaled by default).

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
* equal-to-validator
* not-equal-to-validator
* blank-validator
* not-blank-validator
* true-validator
* false-validator
* type-validator
* string-validator
* boolean-validator
* integer-validator
* symbol-validator
* keyword-validator
* function-validator
* email-validator
* regex-validator
* url-validator
* not-validator
* and-validator
* or-validator
* one-of-validator
* less-than-validator
* greater-than-validator
* length-validator
