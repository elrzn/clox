;;;; scanner.lisp

(in-package :clox)

(defparameter +keywords+
  '(("and"    . token.and)
    ("class"  . token.class)
    ("else"   . token.else)
    ("false"  . token.false)
    ("for"    . token.for)
    ("fun"    . token.fun)
    ("if"     . token.if)
    ("nil"    . token.nil)
    ("or"     . token.or)
    ("print"  . token.print)
    ("return" . token.return)
    ("super"  . token.super)
    ("this"   . token.this)
    ("true"   . token.true)
    ("var"    . token.var)
    ("while"  . token.while)))

(defclass/std scanner ()
  ((source :type string :ri)
   (tokens :type (trivial-types:proper-list token) :a)
   (start :type fixnum :std 0 :a)
   (current :type fixnum :std 0 :a)
   (line :type fixnum :std 1 :a)))

(defun make-scanner (source)
  (make-instance 'scanner :source source))

(defmethod append-token ((scanner scanner) (token token))
  (push token (tokens scanner))
  (setf (tokens scanner) (nreverse (tokens scanner))))

(defmethod scan-tokens ((scanner scanner))
  (loop until (is-at-end-p scanner)
        do (progn
             (setf (start scanner) (current scanner))
             (scan-token scanner)))
  (append-token scanner
                (make-token token.eof "" nil (line scanner))))

(defmethod is-at-end-p ((scanner scanner))
  (>= (current scanner) (length (source scanner))))

(defmethod scan-token ((scanner scanner))
  (flet ((token+ (token)
           (add-token scanner token nil)))
    (let ((char% (advance scanner)))
      (case char%
        (#\( (token+ token.left-paren))
        (#\) (token+ token.right-paren))
        (#\{ (token+ token.left-brace))
        (#\} (token+ token.right-brace))
        (#\, (token+ token.comma))
        (#\. (token+ token.dot))
        (#\- (token+ token.minus))
        (#\+ (token+ token.plus))
        (#\; (token+ token.semicolon))
        (#\* (token+ token.star))
        (#\! (token+ (if (match scanner #\=) token.bang-equal token.bang)))
        (#\= (token+ (if (match scanner #\=) token.equal-equal token.equal)))
        (#\< (token+ (if (match scanner #\=) token.less-equal token.less)))
        (#\> (token+ (if (match scanner #\=) token.greater-equal token.greater)))
        (#\/ (if (match scanner #\/)
                 ;; Comments reach until the end of the line.
                 (loop while (and (char/= (peek scanner) #\Newline)
                                  (not (is-at-end-p scanner)))
                       do (advance scanner))
               (token+ token.slash)))
        ;; Ignore whitespace.
        (#\Space nil)
        (#\Tab nil)
        (#\Newline (incf (line scanner)))
        (#\" (scan-string-token scanner))
        (t (cond
            ((digit-char-p char%) (scan-number-token scanner))
            ((alphabeticp char%) (identifier scanner))
            (t (error% (line scanner) "Unexpected character."))))))))

(defmethod advance ((scanner scanner))
  (with-slots (source current)
      scanner
    (incf current)
    (char source (1- current))))

(defmethod add-token ((scanner scanner) (token-type token-type) literal)
  (with-slots (source start current line)
      scanner
    (let ((text (subseq source start current)))
      (append-token scanner (make-token token-type text literal line)))))

(defmethod match ((scanner scanner) (expected-char character))
  (with-slots (source current)
      scanner
    (cond ((is-at-end-p scanner) nil)
          ((not (char= (char source current)
                       expected-char)) nil)
          (t (incf current)))))

(defmethod peek ((scanner scanner))
  ;; Don't bother with the '\0' case, it does not make sense in Lisp.
  ;; Just treat it as nil.
  (when (not (is-at-end-p scanner))
    (char (source scanner) (current scanner))))

(defmethod scan-string-token ((scanner scanner))
  "Consumes characters until it hits the delimiter of the string. Will
signal an error in case of an abrupt end of input before the string is
closed."
  (with-slots (source start current line)
      scanner
    (loop while (and (char/= (peek scanner) #\")
                     (not (is-at-end-p scanner)))
          when (char= (peek scanner) #\Newline)
          do (incf line)
          do (advance scanner))
    ;; Unterminated string error.
    (if (is-at-end-p scanner)
        (error% line "Unterminated string.")
      (progn
        (advance scanner) ;; Closing dot.
        (add-token scanner
                   token.string
                   (subseq source (1+ start) current))))))

(defmethod scan-number-token ((scanner scanner))
  "It consumes as many digits as it finds for the integer part of the
literal. Then it looks for a fractional part, which is a decimal point
`.` followed by at least one digit."
  (loop while (digit-char-p (peek scanner))
        do (advance scanner))
  (when (and (char= (peek scanner) #\.)
             (digit-char-p (peek-next scanner)))
    (advance scanner)
    (loop while (digit-char-p (peek scanner))
          do (advance scanner)))
  (with-slots (source start current)
      scanner
    (add-token scanner
               token.number
               (parse-number (subseq source start current)))))

(defmethod peek-next ((scanner scanner))
  (with-slots (current source)
      scanner
    (let ((index (1+ current)))
      (when (not (>= index (length source)))
        (char source index)))))

(defmethod identifier ((scanner scanner))
  (with-slots (start current source)
      scanner
    (loop while (alphanumericp (peek scanner))
          do (advance scanner))
    (let* ((text (subseq source start current))
           (token-type (assoc-value +keywords+ text :test #'equal)))
      (add-token scanner token.identifier (or token-type token.identifier)))))

(defun alphabeticp (character)
  (or (alpha-char-p character)
      (char= character #\_)))
