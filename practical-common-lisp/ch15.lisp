#|
Ch. 15: Portable Pathname Library

|#

;; Pre-requisites: Understanding implementation-specific details

;; read-time conditionalization

;; #+ vs. #-
;; #+ : reader reads next expression normally
;; #- : skips the following expression if the following reader expression is true
;;

(defun foo ()
  #+allegro (do-one-thing)
  #+sbcl (do-another-thing)
  #+clisp (something-else)
  #+cmu (yet-another-version)
  #-(or allegro sbcl clisp cmu) (error "Not implemented"))

(defun foo ()
  (do-another-thing))

(defun foo ()
  (do-one-thing))

#|
listing a directory

- can be written as a thin wrapper around DIRECTORY function
- directory
  - takes a special kind of pathname called wild pathname that has one more
    components containing :wild special value
  - if wild path detected, directory will return a list of pathnames in the
    filesystem that matches particular wild pathname.
  - Pattern matching algorithm is NOT defined by language standard, however
    most implementations follow the same basic scheme.
|#

;; naive use of listing files with DIRECTORY and wild pathnames
(print (directory (make-pathname :name :wild :type :wild :defaults "/Users/vedam/")))


#|
Problems with the previous approach
- if path is in file form (i.e. forgetting the trailing /) will cause expression
  to list all files in /Users (treating the last component as the name of 
  file instead of the name of a directory
|#

;; some helper functions to help simplify list-directories
(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p  (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))))

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
      (make-pathname
       :directory (append (or (pathname-directory pathname) (list :relative))
                          (list (file-namestring pathname)))
       :name      nil
       :type      nil
       :defaults pathname)
      pathname)))

(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
      (let* ((directory (pathname-directory pathname))
             (name-and-type (pathname (first (last directory)))))
        (make-pathname
         :directory (butlast directory)
         :name (pathname-name name-and-type)
         :type (pathname-type name-and-type)
         :defaults pathname))
      pathname)))

;; Due to a quirk in CLISP (at the time of writing), DIRECTORY won't return
;; files with no extension unless the type component of the wildcard in NIL
;; rather than :wild. So directory-wildcard is a function that returns the
;; appropriate wildcard for that case

(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

;; first crack at list-directory
(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (directory (directory-wildcard dirname)))

#|
Following implementation would work on:
- SBCL
- CMUCL
- LispWorks

Problems with above implementation
- different implementations may not return subdirectories of given directory
  - Those implementations that do:
    - Allegro, SBCL, CMUCL, and LispWorks
  - Those that don't (by default):
    - OpenMCL (will return the appropriate subdirectories if you pass a true
      value to :directories)
    - CLISP issue (discussed above)
|#

;; portable implementation
(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))

    #+(or sbcl cmu lispworks)
    (directory wildcard)

    #+openmcl
    (directory wildcard :directories t)

    #+allegro
    (directory wildcard :directories-are-files nil)

    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))

;;
;; CL-FAD
;; In general: use UIOP for File I/O.
;;

;; Serapeum : Another utilities library which IGNORING which allows you to
;; have finer-control over errors you would be

;;
;; Lesson Learned
;;
;; Do Not Use ignore-errors never EVER in a library
;;

#|
PROBE-FILE

using probe-file for file existence also has certain implementation specific
issues to deal with
|#

;; example of portable 
(defun file-exists-p (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)

  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  (or (ignore-errors
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))

  #-(or sbcl cmu lispworks openmcl allegro clisp)
  (error "file-exists-p not implemented"))

;;
(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (when (and directories (funcall test name))
              (funcall fn name))
            (dolist (x (list-directory name)) (walk x)))
           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))
