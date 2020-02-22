;;;; scanner.lisp

(in-package :clox)

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
    (case (advance scanner)
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
      (t (error% (line scanner) "Unexpected character.")))))

(defmethod advance ((scanner scanner))
  (incf (current scanner))
  (char (source scanner) (1- (current scanner))))

(defmethod add-token ((scanner scanner) (token-type token-type) literal)
  (let ((text (subseq (source scanner) (start scanner) (current scanner))))
    (append-token scanner (make-token token-type text literal (line scanner)))))

(defmethod match ((scanner scanner) (expected-char character))
  (cond ((is-at-end-p scanner) nil)
        ((not (char= (char (source scanner) (current scanner))
                    expected-char)) nil)
        (t (incf (current scanner)))))

(defmethod peek ((scanner scanner))
  ;; Don't bother with the '\0' case, it does not make sense in Lisp.
  ;; Just treat it as nil.
  (when (not (is-at-end-p scanner))
    (char (source scanner) (current scanner))))

(defmethod scan-string-token ((scanner scanner))
  (loop while (and (char/= (peek scanner) #\")
                   (not (is-at-end-p scanner)))
        when (char= (peek scanner) #\Newline)
        do (incf (line scanner))
        do (advance scanner))
  ;; Unterminated string error.
  (if (is-at-end-p scanner)
      (error% (line scanner) "Unterminated string.")
    (progn
      (advance scanner) ;; Closing dot.
      (add-token scanner
                 token.string
                 (subseq (source scanner)
                         (1+ (start scanner))
                         (current scanner))))))