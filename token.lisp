;;;; token.lisp

(in-package #:clox)

(defdata token-type
  ;; Single-character tokens.
  token.left-paren
  token.right-paren
  token.left-brace
  token.right-brace
  token.comma
  token.dot
  token.minus
  token.plus
  token.semicolon
  token.slash
  token.star
  ;; One or two character tokens.
  token.bang
  token.bang-equal
  token.equal
  token.equal-equal
  token.greater
  token.greater-equal
  token.less
  token.less-equal
  ;; Literals.
  token.identifier
  token.string
  token.number
  ;; Keywords.
  token.and
  token.class
  token.else
  token.false
  token.fun
  token.for
  token.if
  token.nil
  token.or
  token.print
  token.return
  token.super
  token.this
  token.true
  token.var
  token.while
  token.eof)

(defclass token ()
  ((type%   :reader type%   :initarg :type    :initform nil :type token-type)
   (lexeme  :reader lexeme  :initarg :lexeme  :initform nil :type string)
   (literal :reader literal :initarg :literal :initform nil)
   (line    :reader line    :initarg :line    :initform nil :type integer)))

(defmethod print-object ((token token) out)
  (print-unreadable-object (token out :type t)
    (format out "~a ~a ~a"
            (type% token)
            (lexeme token)
            (literal token))))

(defun make-token (token-type lexeme literal line)
  (make-instance 'token
                 :type token-type
                 :lexeme lexeme
                 :literal literal
                 :line line))
