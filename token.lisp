;;;; token.lisp

(in-package #:clox)

(defclass/std token ()
  ((type% :type token-type :ri)
   (lexeme :type string :ri)
   (literal :ri)
   (line :type integer :ri)))

(defmethod print-object ((token token) out)
  (print-unreadable-object (token out :type t)
    (format out "~a ~a ~a"
            (type% token)
            (lexeme token)
            (literal token))))

(defun make-token (token-type lexeme literal line)
  (make-instance 'token
                 :type% token-type
                 :lexeme lexeme
                 :literal literal
                 :line line))
