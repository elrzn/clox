;;;; ast.lisp

(in-package :clox)

(defparameter *types*
  '((binary   . ((expr left) (token operator) (expr right)))
    (grouping . ((expr expression)))
    (literal  . ((object value)))
    (unary    . ((token operator) (expr right)))))

(defmacro define-ast (base-name)
  `(defclass ,base-name () ()))

(defun define-type (base-name class-name fields)
  (eval `(defclass ,class-name (,base-name)
           ,(mapcar (lambda (field)
                      `(,@(cdr field) :type ,(car field))) fields))))

(defun generate (base-name types)
  (define-ast base-name)
  (dolist (type types)
    (let ((class-name (car type))
          (fields (cdr type)))
      (define-type base-name class-name fields))))
