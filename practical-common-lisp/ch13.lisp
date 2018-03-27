;; example of copy-tree
(defparameter *tree-example* '((1 2 (3 4)) (5 6 (7 8))))


;; substitute - list
;; subst - tree

;; example of using subst (returns copy of data structure)
(let ((tcopy (copy-tree *tree-example*)))
  (nsubst 10 2 tcopy)
  (print tcopy))

(print *tree-example*)

;; another example of subst
(subst '(a . cons) '(old . pair)
        '((old . spice) ((old . shoes) old . pair) (old . pair))
        :test #'equal)

;; nsubst-if example
(let ((tcopy (copy-tree *tree-example*)))
  (nsubst-if 'X (lambda (x) (tree-equal x '(1 2 (3 4)))) *tree-example*)
  (print tcopy))

(print *tree-example*)

;; you can treat any list as set
;; CL provides many set-theoretic operations that work on lists

(defparameter *one-set* ())
(defparameter *another-set* '(1 2 3))

;; ADJOIN : item -> list -> list
(print (adjoin 1 *one-set*))
(print *one-set*)

(setf *one-set* (adjoin 1 *one-set*))
;; use pushnew if you want to add new members into a set
(pushnew 1 *one-set*)
(pushnew 2 *one-set*)
(pushnew 4 *one-set*)

(print *one-set*)
(print *another-set*)

;; we have union, intersection and set-difference
(print (union *one-set* *another-set*))
(print (intersection *one-set* *another-set*))
(print (set-difference *one-set* *another-set*))
(print (set-difference *another-set* *one-set*))
(print (set-exclusive-or *one-set* *another-set*))

;; MEMBER for set membership
;; returns the cons cell containing the value (i.e. sublist)
(dolist (x *one-set*)
  (print (member x *one-set*)))

(print (member 5 *one-set*))

;; member-if, member-if-not all available
(print (member-if #'numberp '(d a 2 3 b)))

;; associative lists and property-lists
(defparameter *alist* '((A . 1) (B . 2)))

;; add named-value pairs to the front of list using cons
(cons (cons 'C 3) *alist*)
(setf *alist* (cons (cons 'C 3) *alist*))
(print *alist*)

;; acons == (cons (cons 'key 'val) *list*)
(acons 'D 4 *alist*)

;; like CONS, ACONS does not modify the list (must explicitly use SETF)
(setf *alist* (acons 'D 4 *alist*))
(print *alist*)

;; use assoc for membership as 
(print (assoc 'a *alist*))

(print (assoc 'e *alist*))

(print (rassoc 2 *alist*))

(defparameter *string-alist* '(("a" . 1) ("b" . 2)))

(acons "d" 4 *string-alist*)
(print (assoc "a" *string-alist* :test #'string=))

(defparameter *plist* '(a 1 b 2 c 3))

;;
(print (getf *plist* 'a))

;; remf
(remf *plist* 'a)

(defparameter *plist* '(:a 1 :b 2 :c 3))

(print (getf *plist* :a))

(destructuring-bind (&key a b c) '(:a 1 :b 2 :c 3)
  (list :x a :y b :z c))

