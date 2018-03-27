#|
Ch 17: Object Reorientation: Classes

- consists of two major families:
  - built-in: INTEGER, STRING, LIST, etc.
  - user-defined classes 

- Built-In classes
  - cannot sub-class built-in types 
  - can define methods that specialize on these types (i.e. extension methods)

|#

;; As mentioned last time, we define user-defined classes using DEFCLASS

;; basic structure
;;
;; (defclass name (direct-superclass-name*)
;;   (slot-specifier*))
;;

(defclass point ()
  (
