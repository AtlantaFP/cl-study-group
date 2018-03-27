;;;;;;;;;;;;;;;;;;;;
;;
;; Hash Tables
;;
;;;;;;;;;;;;;;;;;;;;

;; make-hash table generates a new hash table
;; with no arguments it leverages the eql which tests
;; to see if two strings are the same object
(defparameter *a-hash-table* (make-hash-table ))

;; eq, eql, equal, equalp

;; use equal if as the test function if you want to use strings as keys
(defparameter *string-key-hash-table* (make-hash-table :test #'equal))

;; one can also use the other standard test object comparison functions
;; eq or equalp

;; WARNING: one can only leverage the standard comparison functions with
;; hash tables

(defun compare-a-with-b (a b)
  (eql a b))

(defparameter *custom-hash-table* (make-hash-table :test #'compare-a-with-b))

;; adding to a phash table is a really a setf on the return value of gethash
(setf (gethash 'foo *a-hash-table*) "test")

;; of course retrieving a value is simply a call to gethash
(gethash 'foo *a-hash-table*)

;; now gethash actually returns multiple-values, the value itself and whether
;; or not the key is present
(gethash 'bar *a-hash-table*)

;; one actually bind the second return value into a variable of it's own using
;; multiple-value-bind
(defun show-value (key hash-table)
  (multiple-value-bind (value present) (gethash key hash-table)
    (if present
	(format nil "Value ~a is present." value)
	(format nil "value ~a not present because key not found." value))))

;; multiple ways of iterating through a hash table
(setf (gethash 'bar *a-hash-table*) "person a")
(setf (gethash 'baz *a-hash-table*) "person b")
(setf (gethash 'bac *a-hash-table*) "person c")

;; Approach 1: maphash
;; fastest by some benchmarks (at least on SBCL)
(maphash #'(lambda (k v)
	     (format t "~a => ~a~%" k v))
	 *a-hash-table*)

;; Approach 2: loop
(loop
   :for k being the hash-keys :in *a-hash-table*
   :using (hash-value v)
   :do (format t "~a => ~a~%" k v))

;; Approach 3: with-hash-table-iterator
(with-hash-table-iterator (iterator *a-hash-table*)
  (loop
       (multiple-value-bind (entry-p key value) (iterator)
	 (if entry-p
	     (format t "~a => ~a~%" key value)
	     (return)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ch. 12 - List Processing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list data structure is built on a more primitive type called a cons cell
;;
(print (cons 1 2))

(defparameter *a-list* (list 1 2 3 4 5))

;; f: x -> y where x is a set of values and y is a set of values

(print (reverse *a-list*))
(print *a-list*)
(setf *a-list* (nreverse *a-list*))
(print (nreverse *a-list*))

;; reason most list functions are written functionally is to share cons
;; cells with their arguments

(defparameter *append-list* (append (list 1 2) (list 3 4)))

;; destructive operations
;; two types : for-side-effects operations and recycling

(defparameter *list-1* (list 1 2))
(defparameter *list-2* (list 3 4))
(defparameter *list-3* (append *list-1* *list-2*))

(setf (first *list-2*) 0) ==> 0
*list-2*                  ==> (0 4)     ; as expected
*list-3*                  ==> (1 2 0 4) ; maybe not what you wanted



