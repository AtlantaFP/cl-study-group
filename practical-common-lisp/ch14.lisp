#|<-- read this line
Ch. 14: Files and File I/O

* Topics discussed this chapter
  - basics of the file library
  - some low-level routines to read/write files 
  - listing files in the filesystem
  - basic of the Common Lisp pathname abstraction
  - discussion
|#

;;; File Library basics

;; open command
;;
;; FUNCTION SIGNATURE:
;; (open filename
;;       &key
;;       (direction :input)
;;       (element-type 'base-char)
;;       if-exists
;;       if-does-not-exist
;;       (external-format :default)
;;       (class 'sb-sy:fd-stream))
;;
;; ADDITIONAL NOTES:
;; if-exists argument one of :
;; '(:SUPERSEDE :APPEND :OVERWRITE :RENAME-AND-DELETE :RENAME :NEW-VERSION :ERROR)
;; if-does-not-exist argument one of:
;; '(:CREATE :ERROR)

(open "~/lisp-projects/common-lisp-the-series/practical-common-lisp/ch14.lisp"
      :direction :input
      :element-type 'base-char)

;;; Reading and Writing
(let ((in (open "~/lisp-projects/common-lisp-the-series/practical-common-lisp/ch14.lisp"
      :direction :input
      :element-type 'base-char)))
  (print (read-line in))
  (close in))

;; example with using if-does-not-exist
(let ((in (open "~/test3.txt" :if-does-not-exist :error)))
  (when in
    (format t "~a~%" (read-line in))
    (close in)))

;; many different reading functions
;;
;; read-line : read an entire string
;; read-char : read a single character
;; read-byte : read single byte
;; read : read an s-expression (this is the R in REPL)
;;
;; read-from-string : read s-expression from string
;; read-preserving-whitespace
;; etc.

(let ((in (open "~/test.txt" :if-does-not-exist nil)))
  (when in
    (format t "~a~%" (read-line in))
    (close in)))



;; example of read
(defparameter *s* (open "~/lisp-projects/common-lisp-the-series/practical-common-lisp/ch14.txt"))

;; example of reading s-expressions from a file
(print (read *s*))
(close *s*)

;; example of single iteration of a read-eval-print
(defparameter *expr* "(+ 1 2 3)")
(print (eval (read-from-string *expr*)))

;; example to write data to file
;;
;; there are bunch of writing functions
;;
;; write-char, write-line, write-string

;; to add new lines to a file
;;
;; terpri - short for terminate print
;; fresh-line - prints newline character unless already at the beginning
;; of a line
;;

;; example combining writing new lines as writing strings to file
(let ((out (open "~/test2.txt"
		 :direction :output
		 :element-type 'base-char
		 :if-exists :supersede
		 :if-does-not-exist :create)))
  (dotimes (x 5)
    (write-line "hello" out)
    (terpri)
    (fresh-line)
    (write-line (write-to-string x) out))
  (close out))


#|
* functions that output Lisp data as s-expressions

 print : s-expression preceded by newline and space
 prin1 : prints just s-expression
 pprint : like prin1 and print but uses the "pretty printer"

* variable *print-readably* used to control what happens 

 for reading binary data
 set :element-type '(unsigned-byte 8)
 use read-byte/write-byte for read/write operations on binary data

* problems with current approach of using let...open...close

 - error-prone : resource leak if you forget to close the file
 - no guarantee close will run if using return or return-from in code
 block
 - pre-emptively jump out of code block due to an error

 * instead of using OPEN / CLOSE, use WITH-OPEN-FILE
|#

;; example shown earlier of writing to file but with WITH-OPEN-FILE
(with-open-file (out "~/test2.txt"
		 :direction :output
		 :element-type 'base-char
		 :if-exists :supersede
		 :if-does-not-exist :create)
  (dotimes (x 5)
    (write-line "hello" out)
    (terpri)
    (fresh-line)
    (write-line (write-to-string x) out)))

#|
Filenames

* Filenames as strings ties code to a particular OS and file system
* CL provides another representation of filenames called Pathname objects
  - supposed to be "portable" way of representing filenames inside CL 
    implementations (more on the quotes later)
* Almost all places in CL where a filename is called for, one can use 
  either Pathnames or namestrings.
* Simple rules to follow (as suggested in PCL)
  - if getting path by application end-user, use namestrings
  - if programmatically generating paths, use pathname objects
* More on Pathnames
    - objects that allow filenames to be manipulated without tying their
      structure to a particular filename syntax.
    - burden of translating between pathnames and local syntax 
      (i.e. namestrings) fall upon the Lisp implementation being used.
    - Consists of 6 components:
        - host
        - device
        - directory
        - name
        - type
        - version
    - Components of Pathname take on atomic values, usually strings
    - Directories are more structured than other components, containing
      list of directory names prefaced with either :absolute or :relative
|#

;; to create a pathname from namestring, use pathname
(defparameter *test-path* (pathname "~/lisp-projects/test.lisp"))

;; use pathname-directory, pathname-name, and pathname-type to access parts of the pathname
;; object
(print (pathname-directory *test-path*))
(print (pathname-name *test-path*))
(print (pathname-type *test-path*))

;; example of constructing relative paths
(make-pathname :directory '(:relative "lisp-projects")
	       :name "ch14"
	       :type "lisp")

n;; use :defaults along with an existing namestring to create new pathnames rather than
;; creating pathname objects from scratch
;; Portable way of creating pathnames if variable used in :defaults is a user-provided namestring
;;
(print (make-pathname :name "ch14" :type "txt" :defaults *test-path*))

;; merge-pathnames used to merge two pathname objects together
(print (merge-pathnames #P"foo/bar.html" #P"html/"))

;; simple of example of using enough-namestring
(defparameter *website* (merge-pathnames #P"foo/bar.html" #P"/www/html/"))
(print (enough-namestring *website* #P"/www/"))

;; can leverage enough-namestring to create a representation of a file in a different root
(defparameter *backup-website* (merge-pathnames
				(enough-namestring *website* #P"/www/")
				#P"/www-backups/"))

;; WARNING: please put the appropriate slash (or backslash in the case of windows at
;; the end of a path if you are writing the namestring of a directory.

;; File form vs Directory Form
(make-pathname :directory '(:absolute "foo") :name "bar") ; file form
(make-pathname :directory '(:absolute "foo" "bar"))       ; directory form

;; use probe-file to check file existence
(print (probe-file (make-pathname :directory '(:absolute :home "test3.txt"))))
(print (probe-file (make-pathname :directory '(:absolute :home)
				  :name "test3"
				  :type "txt")))

