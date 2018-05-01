#|

Programming in the Large: Packages and Symbols

ANNOUNCEMENT: All notes are available at the following github repo:

https://github.com/AtlantaFP/cl-study-group.git

* Agenda
  * Symbols
  * Packages
  * Namespaces

* Sources
  * ANSI Common Lisp by Paul Graham Ch. 8 on Symbols
  * Ch. 21 of Practical Common Lisp
  * Complete Idiot's Guide to Packages (http://www.flownet.com/gat/packages.pdf)

* Additional References
    * Edi Weitz Article on Packages vs Modules vs Systems - http://mirror.informatimago.com/lisp/weitz.de/packages.html

* Symbols
  * built-in data type to represents names in CL
  * Proper structure instance (like created with DEFSTRUCT)
    * symbol accessors
      * SYMBOL-NAME
      * SYMBOL-VALUE
      * SYMBOL-FUNCTION
      * SYMBOL-PACKAGE
      * SYMBOL-PLIST
  * object instances contains information about the different binding 
    associated with symbol object instance
  * Two types of symbols
    * interned symbols
    * uninterned symbols
      * written with #:
  * Some useful functions when dealing with symbols
    * MAKE-SYMBOL
    * FIND-SYMBOL
    * INTERN
    * UNINTERN
    * GENSYM
  * Other types of Symbols
    * Keyword Symbols
      * begin with a colon (:) e.g. :name, :age, etc.
      * interned into a package called KEYWORD and automatically exported
    * Temporary Symbols 
      * generated by GENSYM
|#

;; example of symbol creation with make-symbol
(print (make-symbol "test"))

;; symbols can also be created with spaces in them as well
(print (make-symbol "test symbol"))

;; INTERN finds the existing symbol and if one doesn't exist
;; it creates one and adds it to the current package's symbol table
;; WARNING: using INTERN alone will create unbounded symbols
(intern "square")

;; now that square is intern'ed we can assign values we so choose
(setf (symbol-value 'square) 4)

;; you can also attach lambda functions to the symbol (essentially defining
;; a function)
(setf (symbol-function 'square) (lambda (x) (* x x)))

;; check to see if a symbol is bound by a value
(print (boundp 'square))

;; check to see if a symbol is bound by a function
(print (fboundp 'square))

;; unbind a value from a symbol
(makunbound 'square)

;; unbind a function from a symbol
(fmakunbound 'square)

;; keywords are the same whether quoted or unquoted
(print (eql ':foo :foo))

;; If uninterned, then a new symbol object instance is constructed each time
;; the uninterned symbol is read.
(print (eql '#:foo '#:foo))

#|
Packages

* A collection of name-to-symbol pairs
    * no two symbol instances will share the same name
    * Similar to a module in other programming languages, but with more
      flexibility.
* Symbols found in package when package is loaded are interned into the 
  current package (meaning they are accessible with an additional qualifier)
* Qualified vs Unqualified Names in packages    
* Most names used are "unqualified" meaning they contain no colons
      * When reader reads them in, first translates name to symbol, and 
        then calls INTERN for symbol lookup.
    * Qualified names are names that are interned in a different package
        * Names with a single-colon (:) refer to external symbols (i.e. public API  
          of a package, exported for public-use.)
        * Names with a double-colon (::) refers to any symbol in the given named package
            * WARNING: Except in very RARE cases, please respect the author's decision about 
              the public exposed API use only the external symbols.
* Package Symbol Accessibility
    * present in current package
    * inherited via USE-PACKAGE
      * only external symbols are inherited when package is used
        * a symbol is made external by exporting in a defpackage
    * Only one symbol instance is accessible for a given name in a given package
      * if name conflicts arise, then resolutions can be made by making one of the
        the accessible symbols a shadowing symbol instance, making other instances
        inaccessible.
    * Symbols can be imported into a package
      * added to the current package's name-to-symbol table
    * Symbols can uninterned and removed from symbol table and from shadowing list
* Three Standard Packages
    * CL (i.e. COMMON-LISP)
      * contains all symbols used in standard CL.
      * REPL can't start into this package since new symbols can not be interned into CL
        package.
    * CL-USER (i.e. COMMON-LISP-USER)
      * first package loaded when CL is loaded.
      * used as "scratch" package to intern test symbols into and play with CL.
      * uses CL package
    * KEYWORD
      * package Lisp reader uses to intern names starting with a colon.
|#

(print *package*)

;; different-ways of accessing *package*
(print common-lisp:*package*)
(print cl:*package*)

;; this means the following is interned in CL-USER package and not CL package
(defvar *x* 10)

#|
* What package can do
    * provide basic control over namespaces by controlling how reader translates names
      into symbol objects.
* What Packages DO NOT do
    * don't provide direct control over who can call what function or access what variable
      (packages are dealt with during the reader phase not evaluation)

DEFPACKAGE - used to create new packages
* NOTE: Refer to CLHS for proper BNF grammar. This has been abbreviated to show essential info
* options
    * :nicknames <string> - list one or more aliases for newly defined package
    * :documentation - describes the package created
    * :use package-name* - list the packages whose external symbols are interned into current
                           package
    * :shadow <symbol-name>* - exclude certain symbols from being interned into current package
    * :shadowing-import-from <package-name> <symbol-name>* - shadow <symbol-name>* in current 
      package with symbol object instances defined in <package-name>
    * :import-from <package-name> <symbols>* - imports <symbols> from given <package-name>
    * :export - defines external symbols (i.e. public interface for given package). Symbols
                listed here are found or created in the package defined and then exported.
    * :intern - symbol names are found or created in given package. interacts with :use option
                since inherited symbols can be used rather than re-created as new.             
    * :size (optional) - declares approximate number of symbols expected in given declared package. 
|#

;; some examples

(defpackage :package-1
  (:documentation "first package documentation string")
  (:nicknames :package1)
  (:use :cl)
  (:export
   :hello-world
   :mult
   :div))

(in-package :package1)

(defun hello-world ()
  (format t "hello from package1~%"))

(defun mult (a b)
  (format t "calling package1's mult operation~%")
  (* a b))

(defun div (a b)
  (format "calling package1's div operation~%")
  (/ a b))

(defpackage :package-2
  (:documentation "another package")
  (:nicknames :package2)
  (:use :cl)
  (:export :hello-world
	   :add
	   :sub))

(in-package :package2)

(defun hello-world ()
  (format t "hello from package2~%"))

(defun add (a b)
  (+ a b))

(defun sub (a b)
  (format t "calling package2's sub function")
  (- a b))

(defpackage :package-3
  (:nicknames :package3)
  (:use :cl :package2)
  (:import-from :package1 :mult)
  (:shadowing-import-from :package1 :hello-world)
  (:shadow :sub))

(in-package :package3)

(defun another-function ()
  (format t "calling utils:hello-world from another-function~%")
  (hello-world))

(defun sub (a b)
  (format t "calling package3's sub function")
  (- a b))


#|
Package Gotchas
- When names are first used on REPL and symbol mapping doesn't exist, CL creates a new 
interned symbol that is unbounded (meaning it creates an entry into the table that you can 
further SETF)
    - if loading a package that contains that symbol/name mapping, will cause CL to 
      notify you that there is NAME conflict
- Subtle shadowing errors
- QUIT is implementation-dependent

Final rules of thumb when creating packages
- use of package.lisp 
- use ASDF 
|#
