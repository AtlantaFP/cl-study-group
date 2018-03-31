#|
Ch 17: Object Reorientation: Classes

- consists of two major families:
  - built-in: INTEGER, STRING, LIST, etc.
  - user-defined classes 

- Built-In classes
  - cannot sub-class built-in types 
  - can define methods that specialize on these types (i.e. extension methods)


 As mentioned last time, we define user-defined classes using DEFCLASS

 Below shows the basic structure

 (defclass name (direct-superclass-name*)
   (slot-specifier*))

- Additional Notes
  - Class names in separate namespace from functions and variables (i.e. you
    can have a class, function and variable all with the same name.)
  - class names passed as argument to MAKE-INSTANCE, function used to create
    instances of user-defined classes.
  - All classes directly subclass STANDARD-OBJECT by default. All other user-
    defined classes must be defined in the direct-superclass-names list shown
    in the above format.
  - STANDARD-OBJECT itself is a subclass of T (which is the superclass of all
    classes in Common Lisp). This is what forms the single class hierarchy for
    all classes in CL.
|#

#|
SLOT specifiers
  - equivalent to instance variables in other modern OO languages
  - can be access through accessor methods (generated reader/writer methods) or
    via SLOT-VALUE function
  - SLOT-VALUE can also be used to SETF instance variables
|#

;; simple example - creating a bank account with some slot specifiers
;;
(defclass bank-account ()
  (customer-name
   balance))

(defclass checking-account (bank-account)
  (checking-account-type))

(defclass savings-account (bank-account)
  (savings-interest-rate))

;; now we can instantiate an instance of this class and use SETF and
;; SLOT-VALUE to instantiate the members of the object

(defparameter *bank-account* (make-instance 'bank-account))

(setf (slot-value *bank-account* 'customer-name) "John Doe")
(setf (slot-value *bank-account* 'balance) 1000)

;; can also use SLOT-VALUE to access the value of the slots
(print (slot-value *bank-account* 'customer-name))
(print (slot-value *bank-account* 'balance))

#|

3 ways to control initial value of slots when constructing object instances
1. :initarg
  - specify a name that can then be used as keyword to make-instance
2. :initform
  - specify a LISP expression that will be used to compute a value for the slot
    if no :initarg argument is passed to MAKE-INSTANCE.
3. INITIALIZE-INSTANCE
  - generic function that can be specialized on, called by MAKE-INSTANCE 
    internally
4: :default-initargs
  - useful when sub-classing a class

- Most of the time :initarg/:initform is sufficient enough for object 
  initialization
- If really need finer-control over object initialization (say for computed
  properties) must use INITIALIZE-INSTANCE

Google "default-initargs vs initform common lisp" for more information on advantages of using default-initargs
:default-initargs, :initarg/:initform, initialize-instance
|#

;; Option 1 and 2 (:initarg and :initform)
(defparameter *account-numbers* 0)

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "CUSTOMER-NAME must be passed in with initial value")
    :reader customer-name
    :accessor the-customer)
   (balance
    :initarg :balance
    :initform 0)
   (account-number
    :initform (incf *account-numbers*))
   account-type))

;; Most common way is to define an :AFTER method on initialize-instance
;; so as not to disturb the primary method of leveraging the :initarg and
;; :initform options
(defmethod initialize-instance :after ((account bank-account) &key)
  (let ((balance (slot-value account 'balance)))
    (setf (slot-value account 'account-type)
	  (cond
	    ((>= balance 100000) :gold)
	    ((>= balance 50000) :silver)
	    (t :bronze)))))

;; can also specify a keyword parameter to INITIALIZE-INSTANCE (parameter
;; is exposed in MAKE-INSTANCE as well)
(defmethod initialize-instance :after ((account bank-account)
				       &key opening-bonus-percentage)
  (when opening-bonus-percentage
    (incf (slot-value account 'balance)
	  (* (slot-value account 'balance) (/ opening-bonus-percentage 100))))
  (let ((balance (slot-value account 'balance)))
    (setf (slot-value account 'account-type)
	  (cond
	    ((>= balance 100000) :gold)
	    ((>= balance 50000) :silver)
	    (t :bronze)))))

;;; Accessor Functions

#|

Limitations of using SLOT-VALUE to access instance variables
- use leads to tight-coupling between implementation and code that uses it
  (i.e. violates coding to interface not implementation)
- does not limit the ways outside code can modify a particular slot 
  (i.e. using SLOT-VALUE alone makes everything SETF'able)

|#

;; simple example of hiding use SLOT-VALUE
(defun balance (account)
  (slot-value account 'balance))

;; since you may want a global way of printing balances regardless of subclass
;; of bank account, better approach to create generic function and specialization
;; on bank account

(defgeneric balance (account)
  (:documentation "account balance"))

(defmethod balance ((account bank-account))
  (slot-value account 'balance))

;; SETF methods
;; - way to extend SETF, defining a new place SETF will know how to set
;; - name of SETF function is two-item list whose first symbol is always
;; setf and second element is a symbol, typically name of function
;;

(defun (setf customer-name) (name account)
  (setf (slot-value account 'customer-name) name))

;; create SETF'able methods to allow modification of SLOTs using setf

;; like before it's better to write it as a generic function if there may
;; be other specializations or a global method for a class hierarchy
(defgeneric (setf customer-name) (value account)
  (:documentation "modifier for customer-name on objects."))

(defmethod (setf customer-name) (value (account bank-account))
  (setf (slot-value account 'customer-name) value))

;; WITH-SLOTS and WITH-ACCESSORS
(defparameter *minimum-balance* 1000)

(defgeneric assess-low-balance-penalty (account)
  (:documentation "assesses a penalty on a bank account if balance falls below a certain minimum."))

;; original implementation
(defmethod assess-low-balance-penalty ((account bank-account))
  (when (< (slot-value account 'balance) *minimum-balance*)
    (decf (slot-value account 'balance) (* (slot-value account 'balance) .01))))

;; revised implementation (using WITH-SLOTS)
(defmethod assess-low-balance-penalty ((account bank-account))
  (with-slots (balance) account
    (when (< balance *minimum-balance*)
      (decf balance (* balance .01)))))

;; can also a two-item list with WITH-SLOTS when writing your code
;; first item is the variable name
;; second item is the name of the slot it references

(defmethod assess-low-balance-penalty ((account bank-account))
  (with-slots ((bal balance)) account
    (when (< bal *minimum-balance*)
      (decf bal (* bal .01)))))

;; if balance was defined with an :accessor rather than :reader, you
;; also use WITH-ACCESSORS rather than WITH-SLOTS. 
(defmethod assess-low-balance-penalty ((account bank-account))
  (with-accessors ((bal balance) (ty account-type)) account
    (when (< bal *minimum-balance*)
      (decf bal (* bal .01)))))

(defclass foo ()
  ((a :initarg :a :initform "A" :accessor a)
   (b :initarg :b :initform "B" :accessor b)))

(defclass bar (foo)
  ((a :initform (error "Must supply a value for a"))
   (b :initarg :the-b :accessor the-b :allocation :class)))


;; example of multiple inheritance
(defclass money-market-account (checking-account savings-account) ())

#|

Order of object specificity shown below

(money-market-account
 checking-account
 savings-account
 bank-account
 standard-object
 t)

- while there is a way to call up the chain, outside of refactoring
  specific behavior into a separate function, no way of invoking a particular
  "less-specific method".

:before, :after, :around



https://itch.io/jam/lisp-game-jam-2018

#lispgames on FreeNode
#gamedev on Discord

|#
