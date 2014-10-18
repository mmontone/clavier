(defpackage :clavier.test
  (:use :cl :clavier :stefil)
  (:export :clavier-tests))

(in-package :clavier.test)

(in-root-suite)

(defsuite clavier-tests)

(in-suite clavier-tests)

;; Validator errors
(deftest validator-errors-test ()
  (let ((validator (make-instance 'equal-to-validator :object 22)))
    (is (validate validator 22 :error-p t))
    (signals validation-error
      (validate validator 33 :error-p t))
  (collecting-validation-errors (errors found-p)
      (progn
	(funcall validator 33 :error-p t)
	(funcall validator 44 :error-p t))
    (is (equalp (length errors) 2))
    (is found-p))
  (collecting-validation-errors (errors found-p)
      (progn
	(funcall validator 33)
	(funcall validator 44))
    (is (not found-p))
    (is (not errors)))))

;; equal-to validator
(deftest equal-to-validator-test ()
  (let ((validator (make-instance 'equal-to-validator :object 22)))
    (is (validate validator 22))
    (is (funcall validator 22))
    (is (not (funcall validator 33)))
    (multiple-value-bind (result message)
	(funcall validator 33)
      (is (equalp message (validator-message validator 33))))))

;; one-of-validator
(deftest one-of-validator-test ()
  (let ((validator (make-instance 'one-of-validator :options (list "foo" "bar"))))
    (is (funcall validator "foo"))
    (is (funcall validator "bar"))
    (is (not (funcall validator "bye")))))

;; blank validator
(deftest blank-validator-test ()
  (let ((validator (make-instance 'blank-validator)))
    (is (not (funcall validator "foo")))
    (is (funcall validator ""))
    (is (funcall validator nil))))

;; not-blank validator
(deftest not-blank-validator-test ()
  (let ((validator (make-instance 'not-blank-validator)))
    (is (funcall validator "foo"))
    (is (not (funcall validator "")))
    (is (not (funcall validator nil)))))

;; type validator

(deftest type-validator-test ()
  (let ((validator (make-instance 'type-validator :type 'string)))
    (is (not (funcall validator 2)))
    (is (funcall validator "hello")))

  (let ((validator (make-instance 'type-validator :type 'boolean)))
    (is (funcall validator nil))
    (is (funcall validator t))
    (is (not (funcall validator "hello"))))

  (let ((validator (make-instance 'type-validator :type 'integer)))
    (is (funcall validator 22))
    (is (not (funcall validator "hello")))))

;; string validator
(deftest string-validator-test ()
  (let ((validator (make-instance 'string-validator)))
    (is (funcall validator "lala"))
    (is (not (funcall validator 22)))
    (is (not (funcall validator t)))))

;; boolean validator
(deftest boolean-validator-test ()
  (let ((validator (make-instance 'boolean-validator)))
    (is (not (funcall validator "lala")))
    (is (not (funcall validator 22)))
    (is (funcall validator t))
    (is (funcall validator nil))))

;; integer validator
(deftest integer-validator-test ()
  (let ((validator (make-instance 'integer-validator)))
    (is (not (funcall validator "asdf")))
    (is (funcall validator 22))
    (is (not (funcall validator t)))))

;; symbol validator
(deftest symbol-validator-test ()
  (let ((validator (make-instance 'symbol-validator)))
    (is (not (funcall validator "lala")))
    (is (not (funcall validator 22)))
    (is (funcall validator t))
    (is (funcall validator 'foo))
    (is (funcall validator :foo))))

;; keyword validator
(deftest keyword-validator-test ()
  (let ((validator (make-instance 'keyword-validator)))
    (is (not (funcall validator "lala")))
    (is (not (funcall validator 22)))
    (is (not (funcall validator t)))
    (is (not (funcall validator 'foo)))
    (is (funcall validator :foo))))

;; function validator
(deftest function-validator-test ()
  (let ((validator
	 (make-instance 'function-validator
			:function (lambda (x)
				    (equalp x 22))
			:message (lambda (validator object)
				   (format nil "~A is not 22" object)))))
    (is (funcall validator 22))
    (is (not (funcall validator 33)))))

;; true validator
(deftest true-validator-test ()
  (let ((validator (make-instance 'true-validator)))
    (is (funcall validator t))
    (is (not (funcall validator "lala")))
    (is (not (funcall validator nil)))))

;; false validator
(deftest false-validator-test ()
  (let ((validator (make-instance 'false-validator)))
    (is (not (funcall validator t)))
    (is (not (funcall validator "lala")))
    (is (funcall validator nil))))

;; less than validator
(deftest less-than-validator-test ()
  (let ((validator (make-instance 'less-than-validator :number 22)))
    (is (funcall validator 21))
    (is (not (funcall validator 32)))))

;; greater than validator
(deftest greater-than-validator-test ()
  (let ((validator (make-instance 'greater-than-validator :number 22)))
    (is (not (funcall validator 21)))
    (is (funcall validator 32))))

;; not validator
(deftest not-validator-test ()
  (let ((validator (make-instance 'not-validator :validator (make-instance 'not-blank-validator))))
    (is (not (funcall validator "hello")))
    (is (funcall validator "")))

  (let ((validator (make-instance 'not-validator :validator (make-instance 'blank-validator))))
    (is (funcall validator "hello"))
    (is (not (funcall validator "")))))

;; and validator
(deftest and-validator-test ()
  (let ((validator (make-instance 'and-validator
				  :x (make-instance 'greater-than-validator :number 10)
				  :y (make-instance 'less-than-validator :number 20))))
    (is (not (funcall validator 33)))
    (is (funcall validator 15))
    (is (not (funcall validator 9)))))

;; or validator
(deftest or-validator-test ()
  (let ((validator (make-instance 'or-validator
				  :x (make-instance 'greater-than-validator :number 20)
				  :y (make-instance 'less-than-validator :number 10))))
    (is (funcall validator 33))
    (is (not (funcall validator 15)))
    (is (funcall validator 9))))

;; email validator
(deftest email-validator-test ()
  (let ((validator (make-instance 'email-validator)))
    (is (funcall validator "mariano@gmail.com"))
    (is (not (funcall validator "lala")))
    (is (not (funcall validator "@asdf.com")))))

;; regex validator
(deftest regex-validator ()
  (let ((validator (make-instance 'regex-validator :regex "^foo.*$")))
    (is (funcall validator "foo lala"))
    (is (not (funcall validator " foo lala")))))

;; url validator
(deftest url-validator-test ()
  (let ((validator (make-instance 'url-validator)))
    (is (not (funcall validator "localhost")))
    (is (not (funcall validator "google.com")))
    (is (funcall validator "http://www.google.com"))))

;; length validator
(deftest length-validator-test ()
  (let ((validator (make-instance 'length-validator :min 5)))
    (is (not (funcall validator "lala")))
    (is (funcall validator "foobar"))
    (is (funcall validator "12345")))
  (let ((validator (make-instance 'length-validator :max 5)))
    (is (funcall validator "lala"))
    (is (not (funcall validator "foobar")))
    (is (funcall validator "12345")))
  (let ((validator (make-instance 'length-validator :min 2 :max 6)))
    (is (not (funcall validator "a")))
    (is (funcall validator "foo"))
    (is (not (funcall validator "foobarfoo")))))

;; builder tests
(deftest builders-test ()
  (let ((validator (not-blank)))
    (is (not (funcall validator nil)))
    (is (not (funcall validator "")))
    (is (funcall validator "hello")))

  (let ((validator (&& (greater-than 20)
		       (less-than 30))))
    (is (funcall validator 25))
    (is (not (funcall validator 15))))

  (is (funcall (== 100) 100))
  (is (not (funcall (== 100) 90)))

  (let ((validator (|| (&& (greater-than 20)
			   (less-than 30))
		       (|| (&& (greater-than 1)
			       (less-than 10))
			   (== 100)))))
    (is (funcall validator 5))
    (is (not (funcall validator 90)))
    (is (funcall validator 100))
    (is (funcall validator 25))
    (is (not (funcall validator 50)))))

;; bigger example

(defclass person ()
  ((fullname :initarg :fullname
	     :accessor fullname
	     :initform nil)
   (email :initarg :email
	  :accessor email
	  :initform nil)
   (password :initarg :password
	     :accessor password
	     :initform nil)))

(defun validate-person (person)
  (collecting-validation-errors (errors found-p)
      (with-signal-validation-errors ()
	(funcall (not-blank "Fullname is required") (fullname person))
	(funcall (not-blank) (password person) :message "Password is required")
	(funcall (~ (blank)) (email person) :message "Email is required")
	(funcall (valid-email) (email person)))
    errors))

(let ((person (make-instance 'person)))
  (validate-person person))

;; validator collection
(deftest validator-collection-test ()
  (let ((validator (make-instance 'validator-collection :validators
				(list (not-blank "Fullname is required")
				      (fn (lambda (x)
					    (> (length x) 2))
					  "Fullname is too short")))))
    (let ((person (make-instance 'person)))
      (signals validation-error
	(funcall validator (fullname person))))
    
    (let ((person (make-instance 'person :fullname "M")))
      (signals validation-error
	(funcall validator (fullname person))))
    
    (let ((person (make-instance 'person :fullname "Mariano")))
      (finishes (funcall validator (fullname person))))))
