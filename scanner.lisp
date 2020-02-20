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
  (nreverse (tokens scanner)))

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
      (t (error% (line scanner) "Unexpected character.")))))

(defmethod advance ((scanner scanner))
  (incf (current scanner))
  (char (source scanner) (1- (current scanner))))

(defmethod add-token ((scanner scanner) (token-type token-type) literal)
  (let ((text (subseq (source scanner) (start scanner) (current scanner))))
    (append-token scanner (make-token token-type text literal (line scanner)))))
