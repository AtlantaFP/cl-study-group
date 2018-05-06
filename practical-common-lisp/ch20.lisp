#|
Chapter 20: The Special Operators
|#

;; Controlling Evaluation

;; QUOTE, IF, and PROGN

;; QUOTE prevents code from being evaluated
(print (quote
	(progn
	  (+ 2 3)
	(+ 1 2))))

;; IF - most fundamental boolean choice operation
(when (< 4 5)
  (print "hello"))

(unless (> 4 5)
  (print "hello"))

;; PROGN - runs multiple statements sequentially
(progn
  (print "stmt 1")
  (print "stmt 2")
  (print "stmt 3"))

;; let, let*, setq
(let ((x 0)
      (y 1))
  (print (+ x y)))

(let* ((x 1)
       (y (* x 2)))
  (print (list x y)))

;;
;; while setq can be used to access lexical env, so can setf
;; bottom line: setf, for the most part has entirely replace setq
;;
(let ((x 0))
  (setf x (incf x))
  (print x))

;;
;; flet vs. labels
;;
;; - labels allows you to define recursive functions, flet does not
;; - labels allows you to use functions immediately after definition, thus
;;   allowing you to break a inner function further down into a series of
;;   inner functions.
;;
(flet ((my-double (x) (* 2 x))
       (increment-then-double (x) (my-double (1+ x))))
  (print (increment-then-double (my-double 2))))

;; example of labels where function used other inner functions previously
;; defined in the same form.
(labels ((increment-value (x) (incf x))
	 (increment-then-double (x) (* 2 (increment-value x))))
  (print (increment-then-double 4)))

;; simple example of recursive inner function with labels
(labels ((fac (n acc)
	   (if (<= n 1)
	       acc
		 (fac (1- n) (* acc n)))))
  (print (fac 5 1)))

;; symbol-macrolet: like regular macros except they can't take arguments and are
;; referred to as a plain symbol
(let ((a-list (list 1 2 3 4 5)))
  (symbol-macrolet ((firstx (first a-list)))
    (setf firstx 6)
    (print a-list)))

;; can also use define-symbol-macro to do the same thing (but now the symbol is
;; available in the global namespace
(defparameter *things* '(1 2 3 4 5))
(define-symbol-macro things1 (first *things*))
(setf things1 6)
(print *things*)

(defun add (a b)
  (+ a b))

;; foo
;;    - bar
;;        - baz
#|
In cases where we may want to return abruptly (i.e. in the middle of a code block), you can
use a block form instead. In fact, defun is often implemented using a block.
|#

(block head
  (print (+ 1 3))
  (return-from head 'T)
  (format t "this won't be executed"))

;; example of using tagbody (VERY RARELY USED)
(tagbody
   (setf x 0)
 top
   (if (> x 5)
       nil
       (progn
         (format t "~A~%" x)
         (setf x (incf x))
         (go top))))

;; Another great example of using tagbody (implementing classic algorithms from textbooks)
;; compliments of Practical Common Lisp

(defun algorithm-s (n max) ; max is N in Knuth's algorithm
  (let (seen               ; t in Knuth's algorithm
        selected           ; m in Knuth's algorithm
        u                  ; U in Knuth's algorithm
        (records ()))      ; the list where we save the records selected
    (tagbody
     s1
       (setf seen 0)
       (setf selected 0)
     s2
       (setf u (random 1.0))
     s3
       (when (>= (* (- max seen) u) (- n selected)) (go s5))
     s4
       (push seen records)
       (incf selected)
       (incf seen)
       (if (< selected n)
           (go s2)
           (return-from algorithm-s (nreverse records)))
     s5
       (incf seen)
       (go s2))))


;; example of unwinding stack with block and tagbody

;; First with block (using return-from) (attributed to PCL)
(defun foo ()
  (format t "Entering foo~%")
  (block a
    (format t " Entering BLOCK~%")
    (let ((x #'(lambda () (return-from a))))
      (bar x)
      x))
    (format t " Leaving BLOCK~%"))
  (format t "Leaving foo~%"))

(defun bar (fn)
  (format t "  Entering bar~%")
  (baz fn)
  (format t "  Leaving bar~%"))

(defun baz (fn)
  (format t "   Entering baz~%")
  (funcall fn)
  (format t "   Leaving baz~%"))

;; let's redefine foo with tagbody instead (would this work?)
(defun foo ()
  (format t "Entering foo~%")
  (tagbody     
     (format t " Entering TAGBODY~%")
     (bar #'(lambda () (go exit-foo)))
     (format t " Leaving TAGBODY~%")
   exit-foo
     (format t "Leaving foo~%")))

#|
Important Note to consider if you decide on using block and tagbody

- blocks and tagbody have dynamic extent vs. lexical bindings indefinite extent
    - labels are only available as long as you are within the stack depth
      of a calling block or tagbody
    - in short: if you use a label or closure to access something within a
      tagbody or block      
|#



;; (unwind-protect protected-form
;;  cleanup-form*)
(defmacro with-database-connection ((var &rest open-args) &body body)
  `(let ((,var (open-connection ,@open-args)))
    (unwind-protect (progn ,@body)
      (close-connection ,var))))

(with-database-connection (conn :host "foo" :user "scott" :password "tiger")
  (do-stuff conn)
  (do-more-stuff conn)
  )

(print (funcall #'+ (values 1 2) (values 3 4)))
(print (multiple-value-call #'+ (values 1 2) (values 3 4)))
(multiple-value-bind (x y) (values 1 2)
  (+ x y))

(multiple-value-list (values 1 2))

