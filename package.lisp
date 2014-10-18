(defpackage #:clavier
  (:use #:cl)
  (:export #:*signal-validation-errors*
	   #:call-with-signal-validation-errors
	   #:with-signal-validation-errors
	   #:collecting-validation-errors
	   #:validation-error
	   #:validator
	   #:validate
	   #:message
	   #:validator-message

	   ;; Validators
	   
	   #:validator-collection
	   #:equal-to-validator
	   #:not-equal-to-validator
	   #:blank-validator
	   #:not-blank-validator
	   #:type-validator
	   #:string-validator
	   #:boolean-validator
	   #:integer-validator
	   #:symbol-validator
	   #:keyword-validator
	   #:function-validator
	   #:true-validator
	   #:false-validator
	   #:not-validator
	   #:and-validator
	   #:or-validator
	   #:one-of-validator
	   #:less-than-validator
	   #:greater-than-validator
	   #:email-validator
	   #:regex-validator
	   #:url-validator
	   #:length-validator

	   ;; Validator builders
	   #:==
	   #:~=
	   #:one-of
	   #:blank
	   #:not-blank
	   #:is-true
	   #:is-false
	   #:greater-than
	   #:less-than
	   #:~
	   #:&&
	   #:||
	   #:fn
	   #:is-a
	   #:is-a-string
	   #:is-a-boolean
	   #:is-an-integer
	   #:is-a-symbol
	   #:is-a-keyword
	   #:valid-email
	   #:matches-regex
	   #:valid-url
	   #:len))
