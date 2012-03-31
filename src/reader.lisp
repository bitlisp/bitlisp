(in-package #:bitlisp)

(define-parser *parser*
  (:start-symbol sexp)
  (:terminals (lparen rparen symbol integer single-float double-float string quote))

  (sexp
   (lparen sexps rparen (lambda (a b c) (declare (ignore a c)) (nreverse b)))
   atom)

  (sexps
   (sexps sexp (lambda (a b) (cons b a)))
   ())

  (atom
   symbol
   integer
   single-float
   double-float
   string))

(defun whitespace? (char)
  (member char '(#\Space #\Tab #\Newline #\Return #\Linefeed)))

(defun token-terminator? (char)
  (or (whitespace? char)
      (member char '(#\( #\) #\'))))

(defun read-string (stream)
  (assert (char= #\" (read-char stream)) () "Strings must begin with a double quote")
  (loop with buffer = (make-array 16 :element-type 'base-char
                                     :fill-pointer 0
                                     :adjustable t)
        with escaping = nil
        for char = (read-char stream)
        do (cond (escaping
                  (setf escaping nil)
                  (vector-push-extend char buffer))
                 ((char= #\\ char)
                  (setf escaping t))
                 ((char= #\" char)
                  (return buffer))
                 (t (vector-push-extend char buffer)))))

(defun buffer-atom (stream)
  (loop for char = (read-char stream)
        while (whitespace? char)
        finally (unread-char char stream))
  (loop with buffer = (make-array 16 :element-type 'base-char
                                     :fill-pointer 0
                                     :adjustable t)
        for char = (read-char stream nil)
        do (cond ((null char)
                  (return buffer))
                 ((token-terminator? char)
                  (unread-char char stream)
                  (return buffer))
                 (t (vector-push-extend char buffer)))))

(defun buffer-list (stream)
  (loop with buffer = (make-array 16 :element-type 'base-char
                                     :fill-pointer 0
                                     :adjustable t)
        with nesting = 0
        for char = (read-char stream nil)
        do (cond ((null char)
                  (if (= 0 nesting)
                      (return buffer)
                      (error "EOF in incomplete list")))
                 ((char= #\( char)
                  (incf nesting)
                  (vector-push-extend char buffer))
                 ((char= #\) char)
                  (decf nesting)
                  (vector-push-extend char buffer)
                  (when (= 0 nesting)
                    (return buffer)))
                 (t (vector-push-extend char buffer)))))

(defun buffer-sexp (stream)
  (loop for char = (read-char stream)
        while (whitespace? char)
        finally (unread-char char stream))
  (let ((first (read-char stream)))
    (unread-char first stream)
    (case first
      (#\( (buffer-list stream))
      (#\) (error "Unexpected close paren"))
      (t (buffer-atom stream)))))

(defun lexer (&optional (stream *standard-input*))
  (loop for char = (read-char stream nil)
        while char
        when (not (whitespace? char))
          do (cond ((char= #\( char) (return (values 'lparen char)))
                   ((char= #\) char) (return (values 'rparen char)))
                   ((char= #\' char) (return (values 'quote char)))
                   ((char= #\" char)
                    (unread-char char stream)
                    (return (values 'string (read-string stream))))
                   ((digit-char-p char)
                    (unread-char char stream)
                    (let ((number (parse-number (buffer-atom stream))))
                      (return (values (etypecase number
                                        (integer 'integer)
                                        (single-float 'single-float)
                                        (double-float 'double-float))
                                      number))))
                   (t
                    (unread-char char stream)
                    (return (values 'symbol (make-bl-symbol (buffer-atom stream))))))))

(defun bl-read (&optional (stream *standard-input*))
  (with-input-from-string (s (buffer-sexp stream))
   (parse-with-lexer (curry #'lexer s) *parser*)))

(defun bl-read-file (file)
  (with-open-file (stream file)
    (loop :for form := (handler-case (bl-read stream)
                         (end-of-file () nil))
          :while form :collect form)))
