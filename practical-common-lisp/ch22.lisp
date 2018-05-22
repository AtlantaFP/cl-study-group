#|
Ch. 22: Loop for Black Belts

Agenda
* use case of loop
* What can you do with loop
* Parts of the loop macro
* Some basic examples of loop iteration

* Use Case of loop
    * Any large-scale program contains looping of various kinds
        * iterating through collections
        * summing through a range of numbers
        * random-access patterns through a collection
        * etc.
    * Patterns can be detected if one looks at each above case and a generic
      pattern can be constructed so one no longer needs to worry about exactly
      how a item is accessed (or created in the case of ranges of numbers).
    * Similar design goal was the creation of iterator objects in OO-languages

* loop contributions 
    * can express iteration using quasi-English syntax
        * CONSEQUENCE: There are a number of keywords that are synonyms of each other
          allowing one to write code that looks like more idiomatic English for different
          contexts.
    * can step through variables numerically 
    * can step through various data structures (list, vectors, etc.)
    * perform special operations while looping (collect, count, sum, minimize, maximize)
    * execute lisp expressions
    * decide loop termination condition
    * conditionally run and do any of the above
    * In addition:
        * create local variables within a loop
        * specify before/after expressions to run before and after a loop proper    

* structure of loop macro (compliments of CLHS)
(loop (named <name>)?
     <variable-clause>*
   <main-clause>*) ==> result*

<variable-clause> ::= <with-clause> | <initial-final> | for-as-clause
|#


;;; Counting loops

#|
Generic form of counting loop
(loop (:for | :as) <from-where> <to-where> <by-how-much> 
 <main-clause>*)

<from-where> ::= :from | :downfrom | :upfrom
- from-where specifies the initial value for the loop

<to-where> ::= :to | :downto | :upto | :above
- specifies the stopping point

<by-how-much> ::= :by

* Default initial values 
    * start at zero
    * increment by 1 each iteration
    * no termination (or until some other form terminates loop)
    * NOTE: applies only to loops that are perform incremental stepping not decremental stepping
* Additional Notes
    * treat :for/:as equivalently (both do the same thing from 
|#

;; examples of counting upto 10 (all of the following are equivalent) display incrementally
(loop :for i :from 0 :to 10 :do (print i))
(loop :as i :from 0 :to 10 :do (print i))
(loop :for i :upto 10 :do (print i))
(loop :for i :from 0 :upto 10 :do (print i))
(loop :for i :below 11 :do (print i))
(loop :for i :from 0 :below 11 :do (print i))

;; the following uses equal-then iteration (more on this later) mixed with repeat
(loop :repeat 11
   :for i = 0 :then (incf i)
   :do (print i))

;; shown only for illustration purposes
(let ((i 0))
  (loop :repeat 11
     :do
     (print i)
     (incf i)))


;; same thing but decrementing
(loop :for i :from 10 :downto 0 :do (print i))
(loop :as i :from 10 :above -1 :do (print i))
(loop :for i :downfrom 10 :to 0 :do (print i))
(loop :for i :downfrom 10 :downto 0 :do (print i))
(loop :for i :downfrom 10 :above -1 :do (print i))
(loop :repeat 11
   :for i = 10 :then (decf i)
   :do (print i))

;; shown only for illustration purposes
(let ((i 10))
  (loop :repeat 11
     :do
     (print i)
     (decf i)))

;; iterating over a list
(defparameter *simple-list* '(1 2 3 4 5 6 7 8 9 10))

;; example of loop-item in
(loop :for item :in *simple-list* :do (print item))

;; can also utilize a :by clause and pass a function to use to move down to next item
(loop :for item :in *simple-list* :by #'cddr :do (print item))

;; you can also using :on clause to iterate through specific cons cells in a list
(loop :for item :on *simple-list* :do (print item))
(loop :for item :on *simple-list* :by #'cddr :do (print item))

;; if you want to loop over a vector instead of a list, you will have to use :across clause
;; and not :on or :in.
(defparameter *example-vector* #(1 2 3 4 5))
(loop :for x :across *example-vector* :do (print x))

;; if you are iterating over a string object (which is a vector of chars) use :across
(print (loop :for x :across "abcd" :collect x))

#|
Iterating over Hash tables / Packages

(retrieved from PCL

(loop :for <var> :being ( :the | :each ) <things> ( :in | :of ) <hash-or-package> ...)

* differences between Hash table iteration and Package iteration
    * Hash Table
        * <things> = hash-key | hash-value
        * optional <using> sub-clause if you want to use both hash-keys and values
    * Package
        * <things> = symbol | symbols | present-symbol | present-symbols | external-symbol | external-symbols
        * no using sub-clause available

|#

;; example hash table uses strings as keys instead of symbols
(defparameter *student-age-table* (make-hash-table :test #'equal))

;; population of table with some dummy data
(setf (gethash "Ram" *student-age-table*) 34)
(setf (gethash "Dave" *student-age-table*) 22)
(setf (gethash "Steve" *student-age-table*) 17)

;; examples of iterating through the hash table
(loop :for k :being :each :hash-key :of *student-age-table*
   :using (hash-value v) :do (format t "student: ~A, age: ~A~%" k v))

(loop :for k :being :the :hash-key :in *student-age-table*
   :using (hash-value v) :do (format t "student: ~A, age: ~A~%" k v))

;; you can also iterate through each hash-value instead of 
(loop :for v :being :each :hash-value :of *student-age-table*
   :using (hash-key k) :do (format t "student: ~A, age: ~A~%" k v))

(loop :for v :being :the :hash-value :in *student-age-table*
   :using (hash-key k) :do (format t "student: ~A, age: ~A~%" k v))

;; examples of iterating over a package
;; Structure is similar to iterating over a hash-table except
;; 
(ql:quickload "hello-asdf")

;; loop through symbols in
(loop :for sym :being :each :symbol :in :com.rv.utils :do (print sym))
(loop :for sym :being :the :symbols :in :com.rv.utils :do (print sym))
(loop :for sym :being :each :present-symbol :in :com.rv.utils :do (print sym))
(loop :for sym :being :the :present-symbols :in :com.rv.utils :do (print sym))
(loop :for sym :being :each :external-symbol :in :com.rv.utils :do (print sym))
(loop :for sym :being :the :external-symbols :in :com.rv.utils :do (print sym))

#|
Equals-then iteration

* provides full control of iteration
* similar to DO-loop shown previously except the syntax is more conventional (i.e. Algol-ish)

* form of equals-then iteration (from PCL)
(loop :for <var> = <initial-value-form>  [:then step-form] ...)

* Notes
    * each :for clause is evaluated separately in the order it appears
    * if step-forms need to be evaluated before any variables is given it's new value
      replace any subsequent :for clause after first :for with an :and clause
|#

;; examples
(defun powers-of-two (n)
  "Compute the first N powers of 2."
  (loop :repeat (1+ n)
     :for x = 0 :then y
     :for y = 1 :then (+ x y)
     :collect y))

(print (powers-of-two 10))

(defun compute-fibo-seq (n)
  "Returns a sequence of the first N Fibonacci numbers."
  (loop :repeat (1+ n)
     :for x = 0 :then y
     :and y = 1 :then (+ x y)
     :collect y))

(print (compute-fibo-seq 10))

#|
Destructuring variables
* Recall destructuring-bind

(destructuring-bind (a b &optional (c 'cee)) '(1 2)
  (list a b c))
* loop gives you a similar feature to take a part values of list
|#

(loop :for (a b) :in '((1 2) (3 4) (5 6))
   :do (format t "a: ~a; b: ~a~%" a b))

;; we've also seen this as a way of display comma-separated lists
;; recall *simple-list* = (list 1 2 3 4 5 6 7 8 9 10)

(loop :for (item . rest) :on *simple-list*
   :do (format t "~a" item)
   :when rest :do (format t ", "))

#|
Accumulation

* most power part of loop
* provide concise notation for a handful of common looping idioms dealing with accumulating
  values.
* can return accumulation (accumulates a default value and returns it) OR you can save it into
  a local variable (using :into subclause)
* Types of Accumulation
    * Numeric Accumulation (sum/summing, count/counting, maximize/maximizing, minimize/minimizing
    * List Accumulation (collect/collecting, append/appending, nconc/nconcing)

* Numeric Accumulation - deals with return a single value
* List Accumulation - builds up ands returns a list
|#

;; examples
(defun range (a b)
  (loop :for i :from a :to b :collect i))

(defun flatten-list (alist)
  "flatten a list of lists using loop."
  (loop :for item :in alist :nconc item))

(defun flatten-list (alist)
  (loop :for item :in alist
     :nconc item into res
     :finally (return res)))

;; example with auxiliary variables and named
(defun print-report (alist stats-fn)
  (multiple-value-bind (asize asum amin amax) (funcall stats-fn alist)
    (format t "Distribution Report~%")
    (format t "-------------------~%")
    (format t "Distribution: ~A~%" alist)
    (format t "Collection Size: ~A~%" asize)
    (format t "Sum: ~A~%" asum)
    (format t "Min: ~A~%" amin)
    (format t "Max: ~A~%" amax)))
	  
(defun run-stats (alist)
  "Finds bounds for numeric values within a list. Assumes list is unsorted."
  (loop :named range
     :for item :in alist
     :with min-value = (car alist)
     :with max-value = 0
     :do
     (when (< item min-value)
       (setf min-value item))
     (when (> item max-value)
       (setf max-value item))
     :count item into coll-size
     :sum item into coll-sum
     :finally (return-from range (values coll-size coll-sum min-value max-value))))

(defun loop-run-stats (alist)
  (loop
     :for item :in alist
     :minimize item :into min-value
     :maximize item :into max-value
     :count item :into coll-size
     :sum item :into coll-sum
     :finally (return (values coll-size coll-sum min-value max-value))))
