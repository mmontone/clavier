;;;; validator.asd

(asdf:defsystem #:clavier.test
  :serial t
  :description "Clavier tests"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:clavier #:stefil)
  :components ((:file "test"))
  :perform (asdf:test-op (o c)
			 (uiop:symbol-call :clavier.test :clavier-tests)))
