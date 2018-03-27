;; (defun test-+ ()
;;   (and
;;    (= (+ 1 2) 3)
;;    (= (+ 1 2 3) 6)
;;    (= (+ -1 -3) -4)))

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defun test-+ ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

;; (report-result <return-value-of-form> <form>)
(defmacro check (&body forms)
`(combine-results
    ,@(loop for f ipn forms collect `(report-result ,f ',f))))

(defvar *test-name*)

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
    (let ((*test-name* ',name))
      ,@body)))

(deftest test-* ()
  (check
    (= (* 2 3) 6)
    (= (* 2 2) 4)))
