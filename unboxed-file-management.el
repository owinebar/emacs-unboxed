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

(defvar unboxed--async-default-time-out 60
  "Default maximum time allowed for async execution of methods in seconds.")

(defvar unboxed--async-byte-compile-time-out 60
  "Maximum time allowed for byte-compiling in seconds.")

(defvar unboxed--async-byte-compile-autoloads-time-out 600
  "Maximum time allowed for byte-compiling autoloads file in seconds.")

(defun unboxed--byte-compiled-library-name (src)
  "Determine the name of the compiled elisp file SRC."
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

(defun unboxed--make-byte-compile-k (pkg loc src &optional category k)
  "Make a continuation for a byte-compiler job.
Arguments:
  LOC - location of compiled byte file
  SRC - file record for source file
  CATEGORY - name of category the compiled file will belong to
  K - Continuation to invoke with updated installed-file INST"
  (let ((logfile-base (file-name-nondirectory (unboxed--file-file src))))
    (let ((logfile (unboxed--make-install-logfile "compile-log" nil logfile-base))
	  (warnfile (unboxed--make-install-logfile "warnings" nil logfile-base))
	  (msgfile (unboxed--make-install-logfile "messages" nil logfile-base)))
      (lambda (elc-path)
	(when elc-path
	  (let ((inst (unboxed--make-Cinstalled-file src elc-path category pkg))
		(log-text (when logfile (unboxed--check-logfile logfile)))
		(warn-text (when warnfile (unboxed--check-logfile warnfile)))
		(msg-text (when msgfile (unboxed--check-logfile msgfile))))
	    (setf (unboxed-installed-file-created inst)
		  (file-exists-p (expand-file-name elc-path loc)))
	    (unboxed--set-file-log inst log-text)
	    (unboxed--set-file-warnings inst warn-text)
	    (unboxed--set-file-messages inst msg-text)
	    (if k
		(funcall k inst)
	      inst)))))))


(defun unboxed--async-byte-compile-file (pkg src setup-exprs ajq finish-k)
  "Byte-compile FILE in asyncronous sandbox.
Arguments:
  PKG - id of source file's package
  SRC - the elisp source file to compile
  SETUP-EXPRS - list of sexps to execute before byte-compiling, e.g. loading
                compile-time requirements not on the standard load-path
  AJQ - job queue for scheduling the async job
  FINISH-K - continuation to call with the installed-file record"
  (let ((job-id (intern (format "byte-compile--%s-%s"
				pkg
				(file-name-nondirectory src))))
	(elc-path (unboxed--byte-compiled-library-name src))
	program)
    (setq program
	  (unboxed--async-expr
	   result
	   `(progn
	      (require 'bytecomp)
	      ,@setup-exprs
	      (when (file-exists-p ,elc-path)
		(delete-file ,elc-path))
	      (byte-compile-file ,src)
	      (setq result ,elc-path))))
    (if ajq
	(unboxed--simple-schedule ajq program job-id
				  unboxed--async-byte-compile-time-out
				  finish-k)
      (async-start
       `(lambda () ,program)
       ,finish-k))))
      
(defun unboxed--async-byte-compile-files (area pkg dst-cat loc srcs libdirs load-ls setup-exprs ajq k)
  "Byte-compile FILE in asyncronous sandbox.
Arguments:
  AREA - unboxing area
  PKG - package id
  DST-CAT - category of the byte-compiled files
  LOC - directory containing the source files
  SRCS - list of file records source files to compile
  LIBDIRS - paths to add to front of load-path during byte-compilation
  LOAD-LS - files to load prior to compiling, e.g. autoload files
  AJQ - job queue for scheduling the async job
  K - continuation to call when all files have been compiled"
  (let ((sys-lp (unboxed--area-system-load-path area))
	(ls srcs)
	el-name
	el-path
	src
	lp
	finish-k)
    (while load-ls
      (push `(load ,(pop load-ls)) setup-exprs))
    (setq lp (append libdirs sys-lp))
    (push `(setq load-path ',lp) setup-exprs)
    (while ls
      (setq src (pop ls)
	    el-name (file-name-nondirectory (unboxed--file-file src))
	    el-path (expand-file-name el-name loc)
	    finish-k  (unboxed--make-byte-compile-k pkg src dst-cat k))
      (unboxed--async-byte-compile-file pkg el-path setup-exprs ajq finish-k))))

(defun unboxed--async-byte-compile-libraries (db pd files src-cat dst-cat ajq k)
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
  `PD' unboxed-package-desc for package containing FILES
  `FILES' list of file records to compile
  `SRC-CAT' file-category record for category of source files
  `DST-CAT' category name for compiled file
  `AJQ' job queue
  `K' Continuation to invoke with the installation record for one elc file"
  (let ((area (unboxed--sexpr-db-area db))
	(pkg-id (unboxed-package-desc-id pd))
	(pkg-name (unboxed-package-desc-name pd))
	(pkg-version (unboxed-package-desc-version-string pd))
	(pkg-loc (unboxed-package-desc-dir pd))
	(autoloads (unboxed--scoped-autoloads db))
	(lp-libdirs (unboxed--scoped-libdirs db))
	(libdir (unboxed--sexpr-db-category-location db 'library))
	(themedir (unboxed--sexpr-db-category-location db 'theme))
	(infodir (unboxed--sexpr-db-category-location db 'info))
	(datadir (unboxed--sexpr-db-category-location db 'data))
	cat-loc setup-exprs)
    (setq datadir (file-name-concat datadir (symbol-name pkg-id))
	  cat-loc (unboxed-file-category-location src-cat)
	  setup-exprs
	  `((setq unboxed-package-id ',pkg-id
		  unboxed-package-name ,pkg-name
		  unboxed-package-version ,pkg-version
		  unboxed-package-box ',pkg-loc
		  unboxed-library-directory ,libdir
		  unboxed-theme-directory ,themedir
		  unboxed-info-directory ,infodir
		  unboxed-package-data-directory ,datadir)))
    (unboxed--async-byte-compile-files
     area pkg-id dst-cat cat-loc files
     lp-libdirs autoloads setup-exprs
     ajq k)))
      


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

(defun unboxed--basic-category-files-install (category area _pkg pkg-box srcs _data-box)
  "Install by copy into category location.
Arguments:
  CATEGORY - name of category
  AREA - area record
  PKG - id of package
  PKG-BOX - directory containing package files
  SRCS - files to install
  DATA-BOX - path to package data files after unboxing"
  (let* ((cat (unboxed--area-category area category))
	 (cat-loc (unboxed-file-category-location cat)))
    (unboxed--install-list
     srcs
     (lambda (src)
       (unboxed--install-simple-copy pkg-box cat-loc src)))))

(defun unboxed--relative-category-files-install (category area pkg pkg-box srcs _data-box)
  "Install by copy into category location.
Arguments:
  CATEGORY - name of category
  AREA - area record
  PKG - id of package
  PKG-BOX - directory containing package files
  SRCS - files to install
  DATA-BOX - path to package data files after unboxing"
  (let* ((cat (unboxed--area-category area category))
	 (cat-loc (unboxed-file-category-location cat)))
    (unboxed--install-list
     srcs
     (lambda (src)
       (unboxed--install-pkg-relative-copy (pkg-box cat-loc src pkg))))))

(defun unboxed--library-category-files-install (category area _pkg pkg-box srcs data-box)
  "Install by copy into category location.
Arguments:
  CATEGORY - name of category
  AREA - area record
  PKG - id of package
  PKG-BOX - directory containing package files
  SRCS - files to install
  DATA-BOX - path to package data files after unboxing"
  (let* ((cat (unboxed--area-category area category))
	 (cat-loc (unboxed-file-category-location cat)))
    (unboxed--install-list
     srcs
     (lambda (src)
       (unboxed--install-rewriting-library-copy pkg-box cat-loc src data-box)))))

(defun unboxed--info-files-finalize-install (category area files)
  "Finalize installation into info dir file.
Arguments:
  CATEGORY - name of category
  AREA - area record
  FILES - files to add to info dir"
  (let* ((cat (unboxed--area-category area category))
	 (cat-loc (unboxed-file-category-location cat)))
    (let ((ls files)
	  file)
      (while ls
	(setq file (cdr (pop ls)))
	(unboxed--install-info-file-in-dir cat-loc file)))))

(defun unboxed--basic-category-files-remove (category area _pkg _pkg-box files)
  "Remove by deletion from category location.
Arguments:
  CATEGORY - name of category
  AREA - area record
  PKG - id of package
  PKG-BOX - directory containing package files
  FILES - files to remove"
  (let* ((cat (unboxed--area-category area category))
	 (cat-loc (unboxed-file-category-location cat)))
    (unboxed--remove-list
     files
     (lambda (file)
       (unboxed--remove-simple-delete cat-loc file)))))

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


(defun unboxed--basic-files-remove (category area files)
  "Remove by deletion from category location.
Arguments:
  CATEGORY - name of category
  AREA - area record
  FILES - files to remove"
  (let* ((cat (unboxed--area-category area category))
	 (cat-loc (unboxed-file-category-location cat)))
    (unboxed--remove-list
     files
     (lambda (file)
       (unboxed--remove-simple-delete cat-loc file)))))

(defun unboxed--info-files-remove (category area files)
  "Remove by deletion from category location.
Arguments:
  CATEGORY - name of category
  AREA - area record
  FILES - files to remove"
  (let* ((cat (unboxed--area-category area category))
	 (cat-loc (unboxed-file-category-location cat)))
    (unboxed--remove-list
     files
     (lambda (_src dst)
       (unboxed--remove-info-file-from-dir dst-loc dst)
       (unboxed--remove-simple-delete dst-loc dst)))))




;;; these functions are run with all the installed-file structs
;;; produced by a set of packages, after the above installers
;;; have completed for *all* the packages in that set.
;;; Return installed-file structs for any additional files produced
;;; during finalization that must be removed when uninstalling a
;;; package, e.g. elc files
;;;   all-cats is provided since the install locations vary between
;;; system and user package sets.



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


;; FIXME - not correct
(defun unboxed--update-autoloads-file (category area files)
  (let* ((cat (unboxed--area-category area category))
	 (cat-loc (unboxed-file-category-location cat))
	 (area-name (unboxed--area-name area))
	 (autoloads-fn (unboxed--area-autoloads-file area))
	 (autoloads-file (expand-file-name (file-name-concat cat-loc
							     autoloads-fn)))
	 (finish-k
	  (unboxed--make-byte-compile-k
	   (intern (format ":area-%s" area-name))
	   cat-loc
	   src
	   'byte-compiled
	   (lambda (inst)
	     (message "Autoload file\n%s"
		      (let ((inhibit-message t))
			(pp inst)))
	     inst)))
	 result comp-file new-installed)
   (let ((al-buffer (get-file-buffer autoloads-file)))
     (when al-buffer
       (with-current-buffer al-buffer
	 (set-buffer-modified-p nil))
       (kill-buffer al-buffer)))
   (make-directory-autoloads cat-loc autoloads-file)
   (let ((default-directory cat-loc))
     (unboxed--async-byte-compile-file nil
				       autoloads-file
				       nil
				       nil
				       finish-k))))

(defun unboxed--immediate-install-package (area pkg pkg-box cat-files new-box)
  "Install package files in an unboxing area.
Returns alist of category names mapped to alists of source/destination
file pairs.
Arguments:
  AREA - unboxing area record
  PKG - symbol identifying package
  PKG-BOX - directory containing the boxed installation of package
  CAT-FILES - association list mapping category to source file paths
  NEW-BOX - path for residual files not installed elsewhere or ignored"
  (message "Unboxing %s" pkg)
  (let ((ls cat-files)
	(cats (unboxed--area-categories area))
	(q (make-queue))
	pr cat-name files)
    (while ls
      (setq pr (pop ls)
	    cat-name (car pr)
	    files (cdr pr))
      (queue-enqueue
       q
       `(,cat-name
	 .
	 ,(unboxed-install-package-category cat-name area
					    pkg pkg-box
					    files new-box))))
    (queue-all q)))

(defun unboxed--immediate-remove-package (area pkg pkg-box cat-files)
  "Remove package files in an unboxing area.
Returns alist of category names mapped to alists of source/destination
file pairs.
Arguments:
  AREA - unboxing area record
  PKG - symbol identifying package
  PKG-BOX - directory containing the boxed installation of package
  CAT-FILES - association list mapping category to source file paths"
  (message "Reboxing %s" pkg)
  (let ((ls (reverse cat-files))
	(q (make-queue))
	pr cat-name files)
    (while ls
      (setq pr (pop ls)
	    cat-name (car pr)
	    files (cdr pr))
      (queue-enqueue
       q
       `(,cat-name
	 .
	 ,(unboxed-remove-package-category cat-name area
					   pkg pkg-box
					   files new-box))))
    (queue-all q)))


(defun unboxed--immediate-initialize-install-files (area cat-files)
  "Initialize installation of files in an unboxing area.
Returns alist of category names mapped to alists of source/destination
file pairs.
Arguments:
  AREA - unboxing area record
  CAT-FILES - association list mapping category to source file paths"
  (message "Initializing unboxing")
  (let ((ls (reverse cat-files))
	(q (make-queue))
	pr cat-name files)
    (while ls
      (setq pr (pop ls)
	    cat-name (car pr)
	    files (cdr pr))
      (queue-enqueue
       q
       `(,cat-name
	 .
	 ,(unboxed-initialize-install-category-files cat-name area
						    files))))
    (queue-all q)))

(defun unboxed--immediate-finalize-install-files (area cat-files)
  "Finalize installation of files in an unboxing area.
Returns alist of category names mapped to alists of source/destination
file pairs.
Arguments:
  AREA - unboxing area record
  CAT-FILES - association list mapping category to source file paths"
  (message "Finalizing unboxing")
  (let ((ls (reverse cat-files))
	(q (make-queue))
	pr cat-name files)
    (while ls
      (setq pr (pop ls)
	    cat-name (car pr)
	    files (cdr pr))
      (queue-enqueue
       q
       `(,cat-name
	 .
	 ,(unboxed-finalize-install-category-files cat-name area
						   files))))
    (queue-all q)))

(defun unboxed--immediate-initialize-remove-files (area cat-files)
  "Initialize removal of installed files in an unboxing area.
Returns alist of category names mapped to alists of source/destination
file pairs.
Arguments:
  AREA - unboxing area record
  CAT-FILES - association list mapping category to source file paths"
  (message "Initializing reboxing")
  (let ((ls (reverse cat-files))
	(q (make-queue))
	pr cat-name files)
    (while ls
      (setq pr (pop ls)
	    cat-name (car pr)
	    files (cdr pr))
      (queue-enqueue
       q
       `(,cat-name
	 .
	 ,(unboxed-initialize-remove-category-files cat-name area
						    files))))
    (queue-all q)))

(defun unboxed--immediate-finalize-remove-files (area cat-files)
  "Finalize removal of installed files in an unboxing area.
Returns alist of category names mapped to alists of source/destination
file pairs.
Arguments:
  AREA - unboxing area record
  CAT-FILES - association list mapping category to source file paths"
  (message "Finalizing reboxing")
  (let ((ls (reverse cat-files))
	(q (make-queue))
	pr cat-name files)
    (while ls
      (setq pr (pop ls)
	    cat-name (car pr)
	    files (cdr pr))
      (queue-enqueue
       q
       `(,cat-name
	 .
	 ,(unboxed-finalize-remove-category-files cat-name area
						  files))))
    (queue-all q)))

(defmacro unboxed--define-transaction-dispatch-method (name imm-name imm-args &rest body)
  "Define a transaction dispatching method.
Arguments:
  NAME - the function name bound for async running IMM-NAME
  IMM-NAME - the function name that will be bound for the immediate action
  IMM-ARGS - the variables that will be bound for the immediate action (must have 5)
  BODY - the body of the method"
  (let ((pd-var (when (or (memq 'pkg imm-args)
			  (memq 'pkg-box imm-args)
			  (memq 'data-box imm-args)
			  (memq 'srcs imm-args))
		  '((pd (unboxed--))))
	(pkg-var (when (memq 'pkg imm-args)
		   '((pkg (unboxed-package-desc-id pd)))))
	(pkg-box-var (when (memq 'pkg-box imm-args)
		       '((pkg-box (unboxed-package-desc-dir pd)))))
	(data-box-var (when (memq 'data-box imm-args)
			'((data-box
			   (file-name-concat
			    (unboxed--sexpr-db-category-location db 'data)
			    (symbol-name (unboxed-package-desc-id pd)))))))
	(srcs-var (when (memq 'srcs imm-args)
		    '((srcs (mapcar #'unboxed-
	
  (when (/= (length imm-args) 4)
    (signal 'unboxed-invalid-install-signature imm-args))
  (let ((doc (concat (format "Dispatch %s files." category)
		     (apply #'format "
Arguments:
  %s - unboxing area record
  %s - name of package as symbol
  %s - location of source files (old box)
  %s - file paths relative to boxed directory of pkg
  %s - new location of files from PKG for relative loading"
			    (mapcar #'unboxed--format-doc-variable imm-args)))))
    `(progn
       (defun ,name (txn ,@(when (memq 'pkg imm-args) '(pd)))
	 (let ((area (unboxed--sexpr-db-area db)))
	   (
	 ,imm-name ,imm-args ,doc
	      (let ((,dst-loc (unboxed--area-category-location
			       ,(car imm-args) ',category)))
		,@body))
       (defun ,async-name (db pd &optional ajq k timeout)
	 ,@(if (or (null body) (and (null (cdr body)) (atom (car body)) (not (symbolp (car body)))))
	       ;; if body is a simple constant (e.g. nil), don't spawn a process
	       body
	     `((when (null timeout)
		 (setq timeout unboxed--async-default-time-out))
	       (let ((area (unboxed--sexpr-db-area db))
		     (pkg-id (unboxed-package-desc-id pd))
		     (pkg-loc (unboxed-package-desc-dir pd))
		     (cq-srcs (unboxed--db-files-locations
			       (unboxed-package-desc-files pd)))
		     (cat-loc (or (unboxed--sexpr-db-category-location db
								       ',category)
				  (error "Could not find location for category %s"
					 ',category)))
		     (data-loc (or
				(unboxed--sexpr-db-category-location db 'data)
				(error "Could not find location for data category")))
		     (setup-exprs unboxed--unboxed-library-paths-loads)
		     job-id program srcs files)
		 (setq srcs (unboxed--get-cat-queue cq-srcs ',category)
		       srcs (when srcs (queue-all srcs))
		       files (mapcar #'unboxed--file-file srcs)
		       job-id (intern (format "%s--%s-%s" 'install ',category pkg-id))
		       program
		       (unboxed--async-expr
			result
			`(progn
			   ,@setup-exprs
			   (setq result (,',imm-name ,area ',pkg-id ,pkg-loc ',files ,data-loc)))))
		 (if ajq
		     (unboxed--simple-schedule ajq program job-id timeout k)
		   
(defun unboxed--install-package (txn pd ajq k)
  )

(defun unboxed--remove-package (txn pd ajq k)
  )

(defun unboxed--initialize-install-files (txn ajq k)
  )

(defun unboxed--finalize-install-files (txn ajq k)
  )

(defun unboxed--initialize-remove-files (txn ajq k)
  )

(defun unboxed--finalize-remove-files (txn ajq k)
  )

(provide 'unboxed-file-management)

;;; unboxed-file-management.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("ajq-" . "async-job-queue-")("ub-" . "unboxed-")("q-" . "queue-"))
;; End:
