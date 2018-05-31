#|
Alternatives to the Loop Macro

@unxn3rd : Twitter
bugrum: functionalprogramming slack

* Agenda
    * disadvantages of loop 
    * Alternative Libraries
        * iterate (2006)
        * for (~2016)

* Disdvantages of Loop
    * not extensible
    * not lispy enough (requires extra parsing)
|#

;; first let's load the library
(ql:quickload "iterate")
(ql:quickload "for")

;; example example: iterating numbers
(print (loop :for i :upfrom 0 :to 10 :collecting i))
(print (iterate:iter (iterate:for i :from 0 :to 10)
		     (iterate:collect i)))

(defparameter *example-list* (iterate:iterate (iterate:for i :to 10 :by 2)
					     (iterate:collect i)))

(defparameter *example-vector* (map 'vector (lambda (x) x) (loop :for i :upto 10 :by 2 :collect i)))
(defparameter *example-string* "hello")

;; can iterate through indices of collections
(iterate:iter (iterate:for i :index-of-sequence *example-list*)
	      (iterate:for x :in *example-list*)
	      (format t "example-list[~a] : ~a~%" i x))

(iterate:iter (iterate:for i :index-of-vector *example-vector*)
	      (iterate:for x :in-vector *example-vector*)
	      (format t "example-vector[~a] : ~a~%" i x))

(iterate:iter (iterate:for i :index-of-sequence *example-string*)
	      (iterate:for x :in-string *example-string*)
	      (format t "example-string[~a] : ~a~%" i x))

;; example of using previous values
(iterate:iter (iterate:for cur :from 0 :to 10 :by 2)
		 (iterate:for prev :previous cur)
		 (format t "prev: ~a, cur: ~a~%" prev cur))

;; reading files using loop
(with-open-file (strm "test-data-set.txt")
  (loop :for line = (read strm nil)
     :until (null line)
     :do (print line)))

;; with iterate:in-file
(iterate:iter (iterate:for line :in-file "test-data-set.txt" :using #'read-line)
	      (format t "~a~%" line))

;; example of extending
(defpackage :com.rv.demo
  (:use :cl :iterate)
  (:export #:demo))

(in-package :com.rv.demo)

(print (iter (for i to 10 by 2)
      (collect i)))

(defun compute-fibo-seq (n)
  (iter (repeat n)
	(for x initially 0 then y)
	(for prev previous x)
	(for y initially 1 then (+ y prev))
	(collect y)))

(defun compute-fibo-seq-two (n)
  (iter (repeat n)
	(for (values x y) initially (values 0 1) then (values y (+ x y)))
	(collect y)))

(defparameter *test-random-coll* (loop :repeat 20 :collect (random 1000)))

(defparameter *random-value* (random 1000))

(print (iter (for x in *test-random-coll*)
      (minimize x into min)
      (maximize x into max)
      (finally (return (list min max)))))

(print (iter (for x in *test-random-coll*)
	     (finding x minimizing (abs (- *random-value* x)) into min-value)
	     (finding x maximizing (abs (- *random-value* x)) into max-value)
	     (finally (return (list min-value max-value)))))

(defmacro-clause (sum-of-squares expr &optional into var)
  `(sum (* ,expr ,expr) into ,var))

(print (iter (for x from 0 to 10)
	     (sum-of-squares x)))

(print (iter (generate x upfrom 0)
	     (until (> (length coll) 10))
	     (collecting (next x) into coll)
	     (finally (return coll))))
	     
(loop :for i :from 0 :to 20
   com.rv.demo::when (evenp i)
   :collect i)

(iter (for i from 0 to 20)
      (when (evenp i)
	(collect i)))
