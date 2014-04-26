(asdf:defsystem #:clavier
  :serial t
  :description "Clavier: A Common Lisp validation library"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-ppcre
               #:cl-json
               #:parenscript
	       #:closer-mop)
  :components ((:file "package")
               (:file "clavier")))

