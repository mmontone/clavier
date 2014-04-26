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
	   #:valid-email
	   #:matches-regex
	   #:valid-url))
