(asdf:defsystem #:clavier
  :serial t
  :description "Clavier: A Common Lisp validation library"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-ppcre
	       #:closer-mop
	       #:chronicity)
  :components ((:file "package")
               (:file "clavier"))
  :in-order-to ((asdf:test-op 
		 (asdf:test-op :clavier.test))))

