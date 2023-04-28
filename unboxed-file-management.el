;;; unboxed-file-management.el --- file management routines   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Onnie Winebarger

;; Author: Onnie Winebarger
;; Copyright (C) 2023 by Onnie Lynn Winebarger <owinebar@gmail.com>
;; Keywords: extensions, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Functions for installing/removing files, and any other file-management related tasks

;;; Code:

(require 'async-job-queue)
(require 'unboxed-decls)
(require 'unboxed-categories)
(require 'rewriting-pcase)
(require 'queue)

(defvar unboxed--async-byte-compile-time-out 60
  "Maximum time allowed for byte-compiling in seconds.")

(defvar unboxed--async-byte-compile-autoloads-time-out 600
  "Maximum time allowed for byte-compiling autoloads file in seconds.")

(defun unboxed--byte-compiled-library-name (src)
  (if (string= (file-name-extension src) "el")
      (concat src "c")
    (concat src ".elc")))

(defun unboxed--file-grep (re file)
  "Test whether FILE matches regular expression RE."
  (with-temp-buffer
    (insert-file-contents file)
    (string-match-p re (buffer-string))))

(defun unboxed--contains-boxed-sexprs-p (loc file)
  "Test whether FILE may require rewriting.
Arguments:
  LOC - directory of package
  FILE - file path relative to LOC"
  (let ((fn (if (symbolp file)
		(symbol-name file)
	      file)))
    (unboxed--file-grep
     "load-file-name"
     (expand-file-name fn loc))))




(defmacro unboxed--define-buffer-snapper (name buffer-id)
  "Set up snap functions to capture buffers used for automatic reporting.
Arguments:
  NAME - symbol for the snapshot
  BUFFER-ID - name of buffer that will be snapshotted
Creates functions `unboxed--start-<name>-snap' and `unboxed--snap-<name>' that
record the buffer position and the buffer text respectively."
  (let ((start (intern (concat "unboxed--start-" (symbol-name name) "-snap")))
	(snap (intern (concat "unboxed--snap-" (symbol-name name))))
	(buffer-var (intern (concat "unboxed--" (symbol-name name) "-buffer-name")))
	(pos-var (intern (concat "unboxed--" (symbol-name name) "-pos"))))
    `(progn
       (defvar ,buffer-var ,buffer-id)
       (defvar ,pos-var nil)
       (defun ,start ()
	 (setq ,pos-var nil)
	 (when ,buffer-var
	   (let ((b (get-buffer ,buffer-var)))
	     (setq ,pos-var
		   (when b
		     (with-current-buffer b (point))))))
	 ,pos-var)
       (defun ,snap ()
	 (let (r b)
	   (when (and ,buffer-var ,pos-var)
	     (setq b (get-buffer ,buffer-var)
		   r (when b
		       (with-current-buffer b
			 (buffer-substring ,pos-var (point))))))
	   (setq ,pos-var nil)
	   r)))))

(unboxed--define-buffer-snapper log byte-compile-log-buffer)
(unboxed--define-buffer-snapper msgs "*Messages*")
(unboxed--define-buffer-snapper warns "*Warnings*")

(defmacro unboxed--start-snaps (&rest names)
  "Start snapshoting NAMES."
  `(progn
     ,@(mapcar (lambda (name)
		 `(,(intern (concat "unboxed--start-"
				    (symbol-name name)
				    "-snap"))))
	       names)))

(defmacro unboxed--with-snaps (names &rest body)
  "Snapshot NAMES and bind in BODY."
  `(let (
	 ,@(mapcar (lambda (name)
		     `(,name
		       (,(intern (concat "unboxed--snap-"
					 (symbol-name name))))))
		   names)
	 )
     ,@body))
      
(defun unboxed--make-rewrite-boxed-sexprs (replacement-box)
  "Return a sexpr-pred for changing boxed sexprs to REPLACMENT-BOX.
Arguments:
  REPLACEMENT-BOX - the path to the new box for loading expressions
                    relative to the package box"
  (lambda (sexpr)
    (pcase sexpr
      (`(file-name-directory load-file-name) replacement-box)
      (`(file-name-directory (or load-file-name . ,_rest))
       replacement-box)
      (`(file-name-directory
	 (or . ,(and (pred listp)
		     ls
		     (guard (memq 'load-file-name ls)))))
       replacement-box)
      (_ nil))))

(defun unboxed--install-info-file-in-dir (loc file)
  "Install info file FILE in directory LOC.
Creates entry for an unboxed package info file in the dir file 
for unboxed packages"
  (let ((full-path (file-name-concat loc file))
	info-dir log-text)
    (setq info-dir (file-name-directory full-path))
    (with-temp-buffer
      (call-process unboxed-install-info-program nil t nil
		    (shell-quote-argument
		     (concat "--info-dir=" info-dir))
		    (shell-quote-argument file))
      (setq log-text (buffer-string)))
    log-text))

(defun unboxed--remove-info-file-from-dir (loc file)
  "Remove info file FILE in directory LOC.
Removes entry for an unboxed package info file in the dir file 
for unboxed packages"
  (let ((full-path (file-name-concat loc file))
	info-dir log-text)
    (setq info-dir (file-name-directory full-path))
    (with-temp-buffer
      (call-process unboxed-install-info-program nil t nil
		    (shell-quote-argument
		     (format "--info-dir=%s" info-dir))
		    "--remove"
		    (shell-quote-argument file))
      (setq log-text (buffer-string)))
    log-text))

(defun unboxed--install-installed-info-file-in-dir (installed-file)
  "Install info file specified by INSTALLED-FILE.
Creates entry for an unboxed package info file in the dir file 
for unboxed packages"
  (let ((file (symbol-name (unboxed-installed-file-file installed-file)))
	(loc (unboxed-installed-file-category-location
	      installed-file)))
    (unboxed--install-info-file-in-dir loc file)))

(defun unboxed--remove-installed-info-file-in-dir (installed-file)
  "Remove info file specified by INSTALLED-FILE.
Removes entry for an unboxed package info file in the dir file 
for unboxed packages"
  (let ((file (symbol-name (unboxed-installed-file-file installed-file)))
	(loc (unboxed-installed-file-category-location
	      installed-file)))
    (unboxed--install-info-file-in-dir loc file)))

(defun unboxed--make-install-logfile (base pkg-name &optional filename)
  "Make a temporary file for recording log buffers from async process.
The prefix for the temporary file is BASE[-PKG-NAME-][-FILENAME-].
Arguments:
  BASE - prefix of file name
  PKG-NAME - package being installed, or nil if none
  FILENAME - file name being installed, or nil if none"
  (let ((logfile-base (concat base
			      (if pkg-name
				  (concat "-"
					  (symbol-name pkg-name)))
			      (if filename
				  (concat (if pkg-name "--" "-")
					  (file-name-nondirectory filename))
				"")
			      "-"))
	(temporary-file-directory unboxed-temp-directory))
    (make-temp-file logfile-base)))

(defun unboxed--make-byte-compile-k (logfile warnfile msgfile inst elc k)
  "Make a continuation for a byte-compiler job.
Arguments:
  LOGFILE - path to compiler message logfile
  WARNFILE - path to warnings logfile
  MSGFILE - path to message logfile
  INST - installed-file record to update with results from compilation
  ELC - path to the compiled file
  K - Continuation to invoke with updated installed-file INST"
  (lambda (_proc-result)
    (let ((log-text (when logfile (unboxed--check-logfile logfile)))
	  (warn-text (when warnfile (unboxed--check-logfile warnfile)))
	  (msg-text (when msgfile (unboxed--check-logfile msgfile))))
      (when (and inst elc)
	(setf (unboxed-installed-file-created inst)
	      (file-exists-p elc)))
      (unboxed--with-snaps
       (log msgs warns)
       (setq log-text (format "Host\n%s\nCompile Process\n%s" log log-text))
       (setq warn-text (format "Host\n%s\nCompile Process\n%s" warns warn-text))
       (setq msg-text (format "Host\n%s\nCompile Process\n%s" msgs msg-text)))
      (setf (unboxed-installed-file-log inst) log-text)
      (setf (unboxed-installed-file-warnings inst) warn-text)
      (setf (unboxed-installed-file-messages inst) msg-text)
      (when k
	(funcall k inst)))))

(defun unboxed--async-byte-compile-file (area src libdirs load-ls ajq k)
  "Byte-compile FILE in asyncronous sandbox.
Arguments:
  AREA - unboxing area
  FILE - the elisp source file to compile
  LIBDIRS - paths to add to front of load-path during byte-compilation
  LOAD-LS - files to load prior to compiling, e.g. autoload files
  AJQ - job queue for scheduling the async job
  K - continuation to call with the installed-file record."
  (let ((el-name (file-name-nondirectory (unboxed--file-file src)))
	(sys-lp (unboxed--area-system-load-path area))
	logfile-base
	logfile
	warnfile
	msgfile
	lp
	load-sexprs
	el-path
	elc-path
	inst
	job-id
	program
	finish-k)
    (while load-ls
      (push `(load ,(pop load-ls)) load-sexprs))
    (setq logfile-base (file-name-sans-extension el-name)
	  logfile (unboxed--make-install-logfile "compile-log" nil logfile-base)
	  warnfile (unboxed--make-install-logfile "warnings" nil logfile-base)
	  msgfile (unboxed--make-install-logfile "messages" nil logfile-base)
	  lp (append libdirs sys-lp)
	  el-path (expand-file-name el-name)
	  elc-path  (if (string= (file-name-extension el-path) "el")
			(concat el-path "c")
		      (concat el-path ".elc"))
	  inst (unboxed--make-installed-file nil elc-path)
	  job-id (intern (concat "byte-compile--" el-name))
	  finish-k  (unboxed--make-byte-compile-k inst
						  elc-path
						  logfile
						  warnfile
						  msgfile
						  k)
	  program
	  (unboxed--async-expr
	   result
	   `(progn 
	      (require 'bytecomp)
	      (setq load-path ',lp)
	      ,@load-sexprs
	      (when (file-exists-p ,elc-path)
		(delete-file ,elc-path))
	      (byte-compile-file ,el-path)
	      (setq result t))))
    (unboxed--simple-schedule ajq program job-id unboxed--async-byte-compile-time-out finish-k)))

(defun unboxed--async-byte-compile-library (db installed-file ajq k)
  "Byte-compile library file of INSTALLED-FILE in a sandbox.
This function defines the following global symbols during compile, so 
a package may capture their value in an `eval-when-compile' form.
  `unboxed-package' Name of the package being installed as a symbol
  `unboxed-package-version' Version of the package being installed as 
  a string
  `unboxed-package-box' Directory containing the unpacked archive of 
  the package
  `unboxed-library-directory' Directory containing the top-level elisp
  libraries of unboxed packages
  `unboxed-theme-directory' Directory containing theme files from 
  unboxed packages
  `unboxed-info-directory' Directory containing info files from 
  unboxed packages
  `unboxed-package-data-directory' Package-specific directory 
  containing any other installed files from this package.
Arguments:
  `DB' Database
  `INSTALLED-FILE' The record for the source library
  `AJQ' job queue
  `K' Continuation to invoke with the installation record for the elc file"
  (when (unboxed-installed-file-created installed-file)
    (unboxed--start-snaps log msgs warns)
    (let ((area (unboxed--sexpr-db-area db))
	  (pkg-name (unboxed-installed-file-package installed-file))
	  (pkg-version (unboxed-installed-file-version installed-file))
	  (pkg-loc (unboxed-installed-file-package-location installed-file))
	  (cat-loc (unboxed-installed-file-category-location installed-file))
	  (lib (symbol-name (unboxed-installed-file-file installed-file)))
	  (libdir (unboxed--sexpr-db-category-location db 'library))
	  (themedir (unboxed--sexpr-db-category-location db 'theme))
	  (infodir (unboxed--sexpr-db-category-location db 'info))
	  (datadir (unboxed--sexpr-db-category-location db 'data))
	  (elc-installed (unboxed-installed-file-struct-copy installed-file))
	  (autoloads (unboxed--scoped-autoloads db))
	  (lp-libdirs (unboxed--scoped-libdirs db))
	  logfile el-path program
	  el-name elc-name elc-path warnfile msgfile
	  sys-lp lp load-sexprs job-id finish-k)
      (setf (unboxed-installed-file-source elc-installed) installed-file)
      (setq logfile (unboxed--make-install-logfile "compile-log" pkg-name lib)
	    warnfile (unboxed--make-install-logfile "warnings" pkg-name lib)
	    msgfile (unboxed--make-install-logfile "messages" pkg-name lib)
	    datadir (file-name-concat datadir (symbol-name pkg-name))
	    sys-lp (unboxed--area-system-load-path area)
	    lp (append lp-libdirs sys-lp)
	    load-sexprs (mapcar (lambda (alfn) `(load ,alfn)) autoloads)
	    el-name lib
	    elc-name  (if (string= (file-name-extension el-name) "el")
			  (concat el-name "c")
			(concat el-name ".elc"))
	    el-path (file-name-concat cat-loc el-name)
	    elc-path (file-name-concat cat-loc elc-name)
	    job-id (intern (concat "byte-compile-"(symbol-name pkg-name) "--" el-name))
	    finish-k  (unboxed--make-byte-compile-k elc-installed
						    elc-path
						    logfile
						    warnfile
						    msgfile
						    k)
	    program
	    (unboxed--async-expr
	     result
	     `(progn 
		(require 'bytecomp)
		(setq load-path ',lp
		      unboxed-package ',pkg-name
		      unboxed-package-version ',pkg-version
		      unboxed-package-box ',pkg-loc
		      unboxed-library-directory ',libdir
		      unboxed-theme-directory ',themedir
		      unboxed-info-directory ',infodir
		      unboxed-package-data-directory ',datadir)
		     (when (file-exists-p ',elc-path)
		       (delete-file ',elc-path))
		     ',@load-sexprs
		     (byte-compile-file ',el-path)
		     (setq result t))
	     logfile
	     warnfile
	     msgfile))
      (setf (unboxed-installed-file-file elc-installed) (intern elc-name))
      (setf (unboxed-installed-file-category elc-installed) 'byte-compiled)
      (unboxed--simple-schedule ajq program job-id unboxed--async-byte-compile-time-out finish-k))))
      


;;; install-action must take four arguments -
;;;  the package name as a symbol
;;;  the category name as a symbol
;;;  the source file name and
;;;  the location for installed files
;;; install-action returns a list of file names actually installed
;;; relative to the supplied location
(defun unboxed--install-list (files install-action)
  "Install list of FILES of using INSTALL-ACTION.
Returns association list of source and destination paths."
  (let ((ls files)
	(q (make-queue))
	src dst)
    (while ls
      (setq src (pop ls))
      (setq dst (funcall install-action src))
      (queue-enqueue q `(,src . ,dst)))
    (queue-all q)))

(defun unboxed--sexpr-rewriting-copy (src dest sexpr-pred)
  "Rewrite file SRC to DEST using SEXPR-PRED."
  (with-temp-buffer
    (insert-file-contents src)
    (rewriting-pcase--pcase-replace-sexpr sexpr-pred)
    (write-region nil nil dest)))

(defun unboxed--simple-copy (src dest &optional _aux)
  "Copy file SRC to DEST.  AUX data is ignored."
  (copy-file src dest t))

(defun unboxed--install-copy (src-loc dst-loc file copy-action &optional aux)
  "Install file into CAT location using COPY-ACTION and AUX data.
Arguments:
  SRC-LOC - directory of source
  DST-LOC - directory for destination
  FILE - file relative to both SRC-LOC and DST-LOC
  COPY-ACTION - function with arguments (SOURCE DEST AUX)
  AUX - optional data to be passed to COPY-ACTION"
  (let ((dst-file (file-name-nondirectory file))
	dest src)
    (setq dest (file-name-concat dst-loc dst-file)
	  src (file-name-concat src-loc file))
    (funcall copy-action src dest aux)
    dst-file))

(defun unboxed--install-rewriting-library-copy (src-loc dst-loc file new-box)
  "Install file into CAT location with rewriting if needed.
Arguments:
  SRC-LOC - directory of source
  DST-LOC - directory for destination
  FILE - file relative to both SRC-LOC and DST-LOC
  NEW-BOX - the directory package box-relative `load' expressions
            should reference"
  (if (unboxed--contains-boxed-sexprs-p src-loc file)
      (unboxed--install-copy
       src-loc dst-loc file
       #'unboxed--sexpr-rewriting-copy
       (unboxed--make-rewrite-boxed-sexprs new-box))
    (unboxed--install-copy
     src-loc dst-loc file
     #'unboxed--simple-copy)))

(defun unboxed--install-simple-copy (src-loc dst-loc file)
  "Copies file into CAT location.
Arguments:
  SRC-LOC - directory of source
  DST-LOC - directory for destination
  FILE - file relative to both SRC-LOC and DST-LOC"
  (unboxed--install-copy src-loc dst-loc file #'unboxed--simple-copy))

(defun unboxed--install-pkg-relative-copy (src-loc dst-loc file pkg)
  "Install file in package-specific subdirectory of CAT.
Arguments:
  PKG - name of pkg as symbol
  SRC-LOC - directory of source
  DST-LOC - directory for destination
  FILE - file relative to both SRC-LOC and DST-LOC"
  (let ((dst-file (file-name-concat (symbol-name pkg) file))
	dest src)
    (setq dest (file-name-concat dst-loc dst-file)
	  src (file-name-concat src-loc file))
    (when (> (length (file-name-nondirectory file)) 0)
      (make-directory (file-name-directory dest) t))
    (copy-file src dest t)
    dst-file))


;;; files is an association list of source/destination pairs
;;; remove-action takes the same arguments as an install-cation
(defun unboxed--remove-list (files remove-action)
  "Remove list of files FILES from DB using REMOVE-ACTION function.
Arguments:
  FILES - Association list of source/destination file pairs
  REMOVE-ACTION - function of two arguments - source and destination file"
  (let ((ls files)
	(q (make-queue))
	pr dfn)
    (while ls
      (setq pr (pop ls))
      (setq dfn (funcall remove-action (car pr) (cdr pr)))
      (when dfn
	(queue-enqueue q dfn)))
    (queue-all q)))

(defun unboxed--remove-simple-delete (dst-loc dst)
  "Delete DST file in DST-LOC.
Arguments:
  DST-LOC - location of DST
  DST - file to remove"
  (let ((dest (file-name-concat dst-loc (file-name-nondirectory dst))))
    (condition-case nil
	(progn
	  (and (file-exists-p dest)
	       (delete-file dest))
	  dest)
      (error ;; do nothing for now
	 nil))))





;;; these functions are run with all the installed-file structs
;;; produced by a set of packages, after the above installers
;;; have completed for *all* the packages in that set.
;;; Return installed-file structs for any additional files produced
;;; during finalization that must be removed when uninstalling a
;;; package, e.g. elc files
;;;   all-cats is provided since the install locations vary between
;;; system and user package sets.


(eval-and-compile
  (defun unboxed--format-doc-variable (sym)
    "Format a variable name SYM for appearance in a docstring."
    (let ((s (symbol-name sym)))
      (when (= (aref s 0) ?_)
	(setq s (substring s 1)))
      (upcase s))))

;; (defmacro unboxed--define-area-method-definer
;;     (outer-prefix method method-text nargs doc-template)
;;   "Define method definer for an area
;; Arguments:
;;   OUTER-PREFIX - prefix of definer macros
;;   METHOD - name of area method
;;   METHOD-TEXT Verb describing method for lead of macro docstring
;;   NARGS - number of args of method signature
;;   DOC-TEMPLATE - template for method definer docstring"
;;   (let ((macro-name (intern (format "%s-define-%s" outer-prefix method)))
;; 	(multi-macro-name (intern (format "%s-define-multiple-%s" outer-prefix method)))
;; 	(dls-var (cl-gensym "dls")))
;;   `(progn
;;      (defmacro ,macro-name (category name args &rest body)
;;        ,(format "Define an unboxing `%s' method.
;; Arguments:
;;   CATEGORY - category of the method
;;   NAME - the function name that will be bound
;;   ARGS - the variables that will be bound (must have 5)
;;   BODY - the body of the method"
;; 		method)
;;        (when (/= (length args) ,nargs)
;; 	 (signal 'unboxed-invalid-method-signature `(,method ,',args)))
;;        (let ((doc (concat (format ,(concat method-text " of %s files.")
;; 				  category)
;; 			  (apply #'format ,(concat "\n" doc-template)
;; 				 (mapcar #'unboxed--format-doc-variable args)))))
;; 	 `(defun ,',name ,',args ,',doc
;; 		 ,',@body)))
;;      (defmacro ,multi-macro-name (prefix categories args &rest body)
;;        ,(format "Define multiple `%s' methods having the same body.
;; Each method will be named `PREFIX-%s-CATEGORY'.
;; Arguments:
;;   PREFIX - used in generating method names
;;   CATEGORIES - list of category names
;;   ARGS - as in `%s'
;;   BODY - the body of each method definition"
;; 		method method macro-name)
;;        (let ((,dls-var
;; 	      (mapcar (lambda (cat)
;; 			`(,macro-name
;; 			  ,',cat
;; 			  ,',(intern (format "%s--%s"
;; 					     prefix
;; 					     cat))
;; 			  ,',args
;; 			  ('\,@ ,'body)))
;; 		      categories)))
;; 	 `(progn (,'\,@ ,dls-var)))))))

;; (unboxed--define-area-method-definer
;;  unboxed- install "Install" 5
;;  "Arguments:
;;   %s - Unboxing area record
;;   %s - Directory of source files
;;   %s - Directory of destination files
;;   %s - association list of relative source paths to destination paths
;;   %s - continuation taking final list of installed files")


(defmacro unboxed--define-install (category name args dst-loc &rest body)
  "Define an unboxing `install' method.
Arguments:
  CATEGORY - category of the method
  NAME - the function name that will be bound
  ARGS - the variables that will be bound (must have 5)
  DST-LOC - the variable for binding the destination location
  BODY - the body of the method"
  (when (/= (length args) 5)
    (signal 'unboxed-invalid-install-signature args))
  (let ((doc (concat (format "Install  %s files."
			     category)
		     (apply #'format "
Arguments:
  %s - unboxing area record
  %s - name of package as symbol
  %s - location of source files (old box)
  %s - file paths relative to boxed directory of pkg
  %s - new location of files from PKG for relative loading"
			    (mapcar #'unboxed--format-doc-variable args)))))
    `(defun ,name ,args ,doc
	    (let ((,dst-loc (unboxed--area-category-location
			     ,(car args) ',category)))
	      ,@body))))

(defmacro unboxed--define-installers (prefix categories args dst-loc &rest body)
  "Define multiple `install' methods having the same body.
Each method will be named `PREFIX-install-CATEGORY'.
Arguments:
  PREFIX - used in generating method names
  CATEGORIES - list of category names
  ARGS - as in `unboxed--define-install'
  DST-LOC - as in `unboxed--define-install'
  BODY - the body of each method definition"
  `(progn
     ,@(mapcar (lambda (cat)
		 `(unboxed--define-install
		   ,cat
		   ,(intern (format "%s-install-%s"
				    prefix
				    cat))
		   ,args
		   ,dst-loc
		   ,@body))
	       categories)))

;; simple copy into category location
(unboxed--define-installers
 unboxed-
 (theme module info )
 (area _pkg src-loc files _new-box)
 dst-loc
 (unboxed--install-list
  files
  (lambda (src)
    (unboxed--install-simple-copy src-loc dst-loc src))))

;; these explicitly ignore any compiled files
(unboxed--define-installers
 unboxed-
 (byte-compiled native-compiled)
 (area _pkg _src-loc _files _new-box)
 _dst-loc
 nil)

;; copy into library directory, possibly rewriting
(unboxed--define-install
 library
 unboxed--install-library
 (area _pkg src-loc files new-box)
 dst-loc
 (unboxed--install-list
  files
  (lambda (src)
    (unboxed--install-rewriting-library-copy src-loc dst-loc src new-box))))

;; copy into package-specific data directory
(unboxed--define-install
 data
 unboxed--install-data
 (area pkg src-loc files _new-box)
 dst-loc
 (unboxed--install-list
  files
  (lambda (src)
    (unboxed--install-pkg-relative-copy src-loc dst-loc src pkg))))

(defmacro unboxed--define-remove (category name args dst-loc &rest body)
  "Define an unboxing `remove' method.
Arguments:
  CATEGORY - category of the method
  NAME - the function name that will be bound
  ARGS - the variables that will be bound (must have 5)
  DST-LOC - the variable for binding the destination location
  BODY - the body of the method"
  (when (/= (length args) 5)
    (signal 'unboxed-invalid-remove-signature args))
  (let ((doc (concat (format "Remove  %s files."
			     category)
		     (apply #'format "
Arguments:
  %s - unboxing area record
  %s - name of package as symbol
  %s - location of source files (old box)
  %s - file paths relative to boxed directory of pkg
  %s - new location of files from PKG for relative loading"
			    (mapcar #'unboxed--format-doc-variable args)))))
    `(defun ,name ,args ,doc
	    (let ((,dst-loc (unboxed--area-category-location
			     ,(car args) ',category)))
	      ,@body))))

(defmacro unboxed--define-removers (prefix categories args dst-loc &rest body)
  "Define multiple `remove' methods having the same body.
Each method will be named `PREFIX-remove-CATEGORY'.
Arguments:
  PREFIX - used in generating method names
  CATEGORIES - list of category names
  ARGS - as in `unboxed--define-remove'
  DST-LOC - as in `unboxed--define-remove'
  BODY - the body of each method definition"
  `(progn
     ,@(mapcar (lambda (cat)
		 `(unboxed--define-remove
		   ,cat
		   ,(intern (format "%s-remove-%s"
				    prefix
				    cat))
		   ,args
		   ,dst-loc
		   ,@body))
	       categories)))

;; simple delete files from category location
(unboxed--define-removers
 unboxed-
 (theme module library byte-compiled native-compiled data)
 (area _pkg _src-loc files _new-box)
 dst-loc
 (unboxed--remove-list 
  files
  (lambda (_src dst)
    (unboxed--remove-simple-delete dst-loc dst))))

;; for info files, first remove entries from dir file
;; then delete the file
(unboxed--define-remove
 info
 unboxed--remove-info
 (area _pkg _src-loc files _new-box)
 dst-loc
 (unboxed--remove-list
  files
  (lambda (_src dst)
    (unboxed--remove-info-file-from-dir dst-loc dst)
    (unboxed--remove-simple-delete dst-loc dst))))


(defmacro unboxed--define-finalize-install (category name args &rest body)
  "Define an unboxing `finalize-install' method.
Arguments:
  CATEGORY - category of the method
  NAME - the function name that will be bound
  ARGS - the variables that will be bound (must have 5)
  BODY - the body of the method"
  (when (/= (length args) 4)
    (signal 'unboxed-invalid-finalize-install-signature args))
  (let ((doc (concat (format "Finalize installation of %s files."
			     category)
		     (apply #'format "
Arguments:
  %s - Unboxing area record
  %s - Directory of source files
  %s - Directory of destination files
  %s - association list of relative source paths to destination paths"
			    (mapcar #'unboxed--format-doc-variable args)))))
    `(defun ,name ,args ,doc
	    ,@body)))

(defmacro unboxed--define-finalize-installers (prefix categories args &rest body)
  "Define multiple `finalize-install' methods having the same body.
Each method will be named `PREFIX-finalize-install-CATEGORY'.
Arguments:
  PREFIX - used in generating method names
  CATEGORIES - list of category names
  ARGS - as in `unboxed--define-finalize-install'
  BODY - the body of each method definition"
  `(progn
     ,@(mapcar (lambda (cat)
		 `(unboxed--define-finalize-install
		   ,cat
		   ,(intern (format "%s-finalize-install-%s"
				    prefix
				    cat))
		   ,args
		   ,@body))
	       categories)))

(defmacro unboxed--define-finalize-remove (category name args &rest body)
  "Define an unboxing `finalize-remove' method.
Arguments:
  CATEGORY - category of the method
  NAME - the function name that will be bound
  ARGS - the variables that will be bound (must have 5)
  BODY - the body of the method"
  (when (/= (length args) 4)
    (signal 'unboxed-invalid-finalize-remove-signature args))
  (let ((doc (concat (format "Finalize removal of %s files."
			     category)
		     (apply #'format "
Arguments:
  %s - Unboxing area record
  %s - Directory of source files
  %s - Directory of destination files
  %s - association list of relative source paths to destination paths"
			    (mapcar #'unboxed--format-doc-variable args)))))
    `(defun ,name ,args ,doc
	    ,@body)))

(defmacro unboxed--define-finalize-removers (prefix categories args &rest body)
  "Define multiple `finalize-remove' methods having the same body.
Each method will be named `PREFIX-finalize-remove-CATEGORY.'
Arguments:
  PREFIX - used in generating method names
  CATEGORIES - list of category names
  ARGS - as in `unboxed--define-finalize-remove'
  BODY - the body of each method definition"
  `(progn
     ,@(mapcar (lambda (cat)
		 `(unboxed--define-finalize-remove
		   ,cat
		   ,(intern (format "%s-finalize-remove-%s"
				    prefix
				    cat))
		   ,args
		   ,@body))
	       categories)))
								     
								     
(unboxed--define-finalize-installers
 unboxed-
 (byte-compiled native-compiled module data theme)
 (_area _src-loc _dst-loc _files)
 nil)

(unboxed--define-finalize-install
 info unboxed--finalize-install-info (_area _src-loc dst-loc files)
 (let ((ls files)
       file)
   (while ls
     (setq file (cdr (pop ls)))
     (unboxed--install-info-file-in-dir dst-loc file)))
 nil)

(unboxed--define-finalize-removers
 unboxed-
 (byte-compiled native-compiled module data theme library info)
 (_area _src-loc _dst-loc _files)
 nil)


;; rebuild the unboxed library autoloads and byte-compile
;; the libraries
;; (unboxed--define-finalize-install
;;  library unboxed-finalize-install-library (area _src-loc dst-loc files)
;;  (let ((autoloads-fn (unboxed--area-autoloads-file area))
;;        (ls files)
;;        autoloads-file result comp-file new-installed)
;;    (setq autoloads-file (expand-file-name (file-name-concat dst-loc autoloads-fn)))
;;    (let ((al-buffer (get-file-buffer autoloads-file)))
;;      (when al-buffer
;;        (with-current-buffer al-buffer
;; 	 (set-buffer-modified-p nil))
;;        (kill-buffer al-buffer)))
;;    (make-directory-autoloads dst-loc autoloads-file)
;;    (let ((default-directory dst-loc))
;;      (setq result (unboxed--async-byte-compile-file autoloads-file))
;;      (when (and (stringp result) (> (length result) 0))
;;        (message "Compile log for %S\n%s" autoloads-file result))
;;      (while ls
;;        (setq inst (pop ls))
;;        (setq comp-file (unboxed--async-byte-compile-library db inst))
;;        (when comp-file
;; 	 (setq new-installed (nconc comp-file new-installed)))))
;;    new-installed))



	


(provide 'unboxed-file-management)

;;; unboxed-file-management.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("ajq-" . "async-job-queue-")("ub-" . "unboxed-")("q-" . "queue-"))
;; End:
