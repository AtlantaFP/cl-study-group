;; do-primes example from Chapter 8 Practical Common Lisp

(defun primep (number)
  "checks to see if a number is prime."
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  "grabs the next prime number after NUMBER."
  (loop for n from number when (primep n) return n))

(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	(,ending-value-name ,end))
       ((> ,var ending-value-name))
     ,@body)))

(let ((ending-value 0))
  (do-primes (p 0 10)
    (incf ending-value p)))

(do-primes (p 0 10)
    (incf ending-value p))
