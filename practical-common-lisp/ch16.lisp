#|
Chapter 16:
Object Reorientation : Generic Functions

- Agenda:
  - talk about Lisp's OO history
  - A little about the CLOS in general
  - generic functions

- Original LISP pre-dates OO by couple of decades
- Common Lisp Object System (CLOS)
  - Originally was an separate entity that later became part of Common Lisp when
    the language was standardized.
- CLOS contributions
  - MetaObject Protocol or MOP (which we'll discuss later)
  - full OO paradigm similar in nature to Smalltalk with Lisp style semantics

- Intro to CL Object Orientation
  - class-based objects
    - all objects are "instances" of a particular class.
    - class of an object determines internal representation
|#

;; simple example of a class (more on this will be discussed later)
;; writer, accessor
(defclass point  ()
  ((x :initarg :x :initform nil :reader point-x)
   (y :initarg :y :initform nil :reader point-y)))

#|
A little foray into Object Orientation
- All OO languages follow the same methodolgy spawn by Simula
  - behavior associated with classes through methods 
    - also called "instance methods", "member functions", etc.
    - all methods belong to a particular class 
    - model of method invocation is called "message passing"
- Original Lisp OO
    - SEND
        - method used in early lisp object systems to do message passing to particular objects
        - example: (send object 'foo)
        - Design side effects
            - couldn't be used like a normal function
            - had to wrap message send/receive's with lambda to use in higher order functions
- Generic Function
    - fixed issues with normal message passing objects
    - ended up becoming heart of Common Lisp's object system
    - What is a generic function?
        - abstract operation but with no implementation 
        - similar to what we call interface methods (although generic functions are not
          tied to a specific object).
|#

;; simple example of generic function
(defgeneric draw (shape)
  (:documentation "draw the given shape on the screen."))


;; Simple example (compliments of PCL)
(defgeneric withdraw (account amount)
  (:documentation "Withdraw specified amount from the account. Signal error if current balance is less than amount."))

;; use defmethod to define methods that implement the withdraw generic function
(defmethod withdraw ((account bank-account) amount)
  (when (< (balance account) amount)
    (error "Account overdrawn."))
  (decf (balance account) amount))

(defmethod withdraw ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft)))
  (call-next-method))

(defmethod withdraw :before ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft))))

(defmethod withdraw :after ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft))))

(defmethod withdraw :around ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft))))

(defmethod withdraw ((account (eql *account-of-bank-president*)) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (incf (balance account) (embezzle *bank* overdraft)))
    (call-next-method)))


(defgeneric priority (job)
  (:documentation "Return the priority at which the job should be run.")
  (:method-combination + :most-specific-last))

(defclass a ()
  ())

(defclass b (a)
  ())

(defgeneric some-method (obj)
  )

(defmethod some-method ((obj a))
    (print "class a method call"))

(defmethod some-method :before ((obj a))
  (print "class a before method call"))

(defmethod some-method :around ((obj a))
  (call-next-method)
    (print "class a around method call"))

(defmethod some-method :after ((obj a))
  (print "class a after method call"))

(defmethod some-method ((obj b))
  (call-next-method)
  (print "class b method call"))

(defmethod some-method :before ((obj b))
  (print "class b before method call"))

(defmethod some-method :around ((obj b))
  (print "class b around method call")
  (call-next-method))

(defmethod some-method :after ((obj b))
  (print "class b after method call"))

(defgeneric some-progn-method (obj)
  (:documentation "shows off progn method combination.")
  (:method-combination progn :most-specific-last))


(defmethod some-progn-method progn ((obj a))
  (print "class a progn method combo called.")
  5)

(defmethod some-progn-method progn ((obj b))
  (print "class b progn method combo called.")
  6)


(defstruct point x y z)

