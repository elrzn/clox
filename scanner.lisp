;;;; scanner.lisp

(in-package :clox)

(defclass/std scanner ()
  ((source :type string :ri)
   (tokens :type (trivial-types:proper-list token) :r)
   (start :type fixnum :std 0 :a)
   (current :type fixnum :std 0 :a)
   (line :type fixnum :std 1 :a)))

(defun make-scanner (source)
  (make-instance 'scanner :source source))

(defmethod scan-tokens ((scanner scanner))
  (loop until (is-at-end-p scanner)
        do (progn
             (setf (start scanner) (current scanner))
             (scan-token scanner)))
  (push (make-instance 'token
                       :lexeme ""
                       :line (line scanner)
                       :literal nil
                       :type% token.eof)
        (tokens scanner))
  (nreverse (tokens scanner)))

(defmethod is-at-end-p ((scanner scanner))
  (>= (current scanner) (length (source scanner))))
