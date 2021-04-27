(asdf:defsystem #:clavier
    :serial t
    :description "Clavier: A Common Lisp validation library"
    :author "Mariano Montone"
    :license "MIT"
    :homepage "https://github.com/mmontone/clavier"
    :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))            
    :depends-on (#:alexandria
                 #:cl-ppcre
                 #:closer-mop
                 #:chronicity
                 #:cl-fad)
    :components ((:file "package")
                 (:file "clavier"))
    :in-order-to ((asdf:test-op
                   (asdf:test-op :clavier.test))))
