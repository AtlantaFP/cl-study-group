#|
Chapter 19: Beyond Exception Handling - Conditions and Restarts

* Agenda
    * Introduction to Error Handling
    * How do other programming languages handle errors
    * Introduction to the CL condition system
    * (Time permitting) restarts

* Error Handling
    * doesn't necessarily mean bug in system
        * external factors 
            * (lack of data, abrupt network connection, pre-conditions not met)
    * Could cause cascade of failures if not handled properly
    * As the saying goes, getting an error may not be a bug but not handling an error is
      mostly certain one.

* Solutions to handling errors
    * return in an error code and some message
    * exception

* Exceptions
    * Feature available in most programming languages
    * common way to handle errors
        * When error arises one would "throw" an exception object with some information
        * To handle errors, one would "catch" the exception object and handle it.
        * Two-part divison: Handler code is typically separated from code that signals
          the actual error.
    * How things are thrown/caught and how to clean up as you unwind the stack is
      typically implementation dependent

* Introduction to CL condition system
    * Performs a three-way separation
        * code that signals the condition
        * code that handles the condition
        * code that actually restarts execution after condition is handled.

* What are conditions?
    * class of objects
    * instance data carries details about what lead to condition being signalled.
    * DEFINE-CONDITION macro
        * works like DEFCLASS
        * differences from DEFCLASS
            * Unlike classes defined with DEFCLASS, default superclass is CONDITION 
              not STANDARD-OBJECT.
            * MUST specify a :reader or :accessor option for any slot defined for a condition
                * can't use SLOT-VALUE
            * MUST use MAKE-CONDIION not MAKE-INSTANCE
                * must specify arguments for condition construction with :initarg    
                * no further customization can be made with an INITIALIZE-INSTANCE equivalent
    * Specific for error handling
        * SHOULD define your conditions as subclasses of ERROR (itself subclass of CONDITION)
|#

;; example of error condition
(define-condition invalid-argument-error (error)
  ((text :initarg :text :reader text)))

#|

* Signal Errors with the function ERROR
    * Can call this two ways
        * pass already constructed condition object
        * call ERROR with particular condition class to construct as argument along with any
          :initarg that need to be passed to MAKE-CONDITION (which is called underneath)
* If error isn't handled anywhere it'll drop you in the debugger.

|#

;; example of using invalid-argument-error
(defun add (a b)
  (if (not (integerp a))
      (error 'invalid-argument-error :text "a must be an integer")
      (if (not (integerp b))
	  (let ((err-obj (make-condition 'invalid-argument-error :text "b must be an integer")))
	    (error err-obj))
	  (+ a b))))

#|
Condition Handlers
    * must be established for any conditions that are thrown otherwise you will land in
      debugger
    * Process
        * Condition is signaled
        * signaling machinery searches through active condition handlers to find a handler
          that matches the condition being signaled based on the condition's class.
        * Finds the most recently established handler whose type specifier is the compatible
          with the condition being signaled and calls it's handler passing in condition 
          object.
    * Ways to handle condition
        * just return (passes buck onto next signal handler registered)
        * simply unwind stack to where handler was established and run some code.
            * HANDLER-CASE macro is used to establish these kinds of handlers.
    * HANDLER-CASE
        (handler-case <expression>
            <error-clause>*)

    * <error-clause> = (condition-type ([var]) code)
    * body of HANDLER-CASE must be a single expression (must use PROGN for multiple expressions)
|#

(defun check-argument (a)
  (unless (integerp a)
    (error 'invalid-argument-error :text "argument must be an integer")))

(defun add (a b)
  (let ((failed-arguments nil))
    (handler-case (check-argument a)
      (invalid-argument-error ()
	(progn
	  (format t "~A not an integer" a)
	  (setf failed-arguments 't))))
    (handler-case (check-argument b)
      (invalid-argument-error ()
	(progn
	  (format t "~A not an integer" b)
	  (setf failed-arguments 't))))
    (unless failed-arguments
      (+ a b))))

#|
Restarts
    * used to split error handling into two parts
        * actual error recovery is converted into a restart 
        * condition handler invokes appropriate restart based on the case    
    * RESTART-CASE 
|#

(defun perform-add (a b)
  (restart-case (check-argument a)
    (provide-default-value ()
      :report (lambda (stream) (format stream "a was not an integer"))
      :interactive (lambda (stream)
		     (format stream "provide new value for a: ")
		     (setf a (parse-integer (read-line)))))
    (evaluate-to-zero ()
      (setf a 0)))
  (restart-case (check-argument b)
    (provide-default-value ()
      (format t "provide new value for b: ")
      (setf b (parse-integer (read-line))))
    (evaluate-to-zero ()
      (setf b 0)))
  (+ a b))

(defun evaluate-to-zero (c)
  (invoke-restart 'evaluate-to-zero))

(defun provide-value (c)
  (invoke-restart 'provide-default-value))

(defun add (a b)
  (handler-bind ((invalid-argument-error #'provide-value))
    (perform-add a b)))


;; Code referenced from: http://clhs.lisp.se/Body/m_w_smp_.htm#with-simple-restart
(defun read-eval-print-loop (level)
  (with-simple-restart (abort "Exit command level ~D." level)
    (loop
       (with-simple-restart (abort "Return to command level ~D." level)
         (let ((form (prog2 (fresh-line) (read) (fresh-line))))
           (prin1 (eval form)))))))
