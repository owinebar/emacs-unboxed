;;; unboxed-file-management.el --- file management routines
;;; unboxed-file-management.el        -*- lexical-binding: t; -*-

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

(require 'async)
(require 'async-job-queue)
(require 'unboxed-decls)
(require 'unboxed-categories)
(require 'unboxed-rewrite-sexprs)


(defun unboxed--file-grep (re file)
  "Test whether FILE matches regular expression RE."
  (with-temp-buffer
    (insert-file-contents file)
    (string-match-p re (buffer-string))))

(defun unboxed--contains-boxed-sexprs-p (db pd cat file)
  "Test whether FILE matches a sexp referencing the package's location.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  CAT - category name
  FILE - file in package's boxed location"
  (unboxed--file-grep "load-file-name"
			  (expand-file-name
			   file
			   (unboxed-package-desc-dir pd))))



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
      
(defun unboxed--make-rewrite-boxed-sexprs (db pd cat file)
  "Return a sexpr-pred for changing boxed sexprs to PD data directory.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  CAT - category name
  FILE - file in package's boxed location"
  (let ((data-directory-val
	 (list
	  (file-name-concat
	   (unboxed--sexpr-db-category-location db 'data)
	   (symbol-name (unboxed-package-desc-name pd))))))
    (lambda (sexpr)
      (pcase sexpr
	(`(file-name-directory load-file-name) data-directory-val)
	(`(file-name-directory (or load-file-name . ,rest))
	 data-directory-val)
	(`(file-name-directory
	   (or . ,(and (pred listp)
		       ls
		       (guard (memq 'load-file-name ls)))))
	 data-directory-val)
	(_ nil)))))


(defun unboxed--install-info-file-in-dir (installed-file)
  "Install info file from INSTALLED-FILE.
Utility for creating entry for an unboxed package info file in the dir file \
for unboxed packages"
  (let ((file (unboxed-installed-file-file installed-file))
	(loc (unboxed-installed-file-category-location
	      installed-file))
	full-path info-dir info-file log-text)
    (setq full-path (file-name-concat loc file))
    (setq info-dir (file-name-directory full-path))
    (setq info-file (file-name-nondirectory full-path))
    (with-temp-buffer
      (call-process unboxed-install-info-program nil t nil
		    (shell-quote-argument
		     (concat "--info-dir=" info-dir))
		    (shell-quote-argument file))
      (setq log-text (buffer-string)))
    (setf (unboxed-installed-file-log installed-file) log-text))
  nil)

(defun unboxed--remove-info-file-from-dir (installed-file)
  "Remove info file specified in INSTALLED-FILE struct.
Utility for creating entry for an unboxed package info file in the dir file \
for unboxed packages"
  (let ((file (unboxed-installed-file-file installed-file))
	(loc (unboxed-installed-file-category-location
	      installed-file))
	full-path info-dir info-file log-text)
    (setq full-path (file-name-concat loc file))
    (setq info-dir (file-name-directory full-path))
    (setq info-file (file-name-nondirectory full-path))
    (with-temp-buffer
      (call-process unboxed-install-info-program nil t nil
		    (shell-quote-argument
		     (concat "--info-dir=" info-dir))
		    "--remove"
		    (shell-quote-argument file))
      (setq log-text (buffer-string)))
    (setf (unboxed-installed-file-log installed-file) log-text)))

(defun unboxed--make-install-logfile (base pkg-name &optional filename)
  "Make a temporary file to record log buffers from async process with \
prefix of the form BASE[-PKG-NAME-][-FILENAME-].
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

(defun unboxed--async-byte-compile-file (file)
  "Byte-compile FILE in asyncronous sandbox."
  (let ((el-name (file-name-nondirectory file))
	logfile-base
	logfile
	warnfile
	msgfile
	sys-lp
	lp
	load-sexprs
	log-text
	warn-text
	msg-text
	elc-name
	elc-path
	result
	proc
	proc-result)
    (while ls
      (push `(load ,(pop ls)) load-sexprs))
    (setq logfile-base (file-name-sans-extension el-name)
	  logfile (unboxed--make-install-logfile "compile-log" nil logfile-base)
	  warnfile (unboxed--make-install-logfile "warnings" nil logfile-base)
	  msgfile (unboxed--make-install-logfile "messages" nil logfile-base)
	  sys-lp (unboxed--area-system-load-path area)
	  lp (append libdirs sys-lp)
	  el-path (expand-file-name el-name)
	  elc-path  (if (string= (file-name-extension el-path) "el")
			(concat el-path "c")
		      (concat el-path ".elc"))
	  program
	  `(lambda ()
	     (condition-case nil
		 (progn
		   (require 'bytecomp)
		   (setq load-path ',lp)
		   ,@load-sexprs
		   (when (file-exists-p ,elc-path)
		     (delete-file ,elc-path))
		   (byte-compile-file ,el-path))
	       (error nil))
	     (let ((log-buffer (get-buffer byte-compile-log-buffer)))
	       (when log-buffer
		 (with-current-buffer log-buffer
		   (write-region (point-min) (point-max) ,logfile))))
	     (let ((log-buffer (get-buffer "*Warnings*")))
	       (when log-buffer
		 (with-current-buffer log-buffer
		   (write-region nil nil ,warnfile))))
	     (let ((log-buffer (get-buffer "*Messages*")))
	       (when log-buffer
		 (with-current-buffer log-buffer
		   (write-region nil nil ,msgfile))))
	     t))
    (setq proc (async-start program nil))
    (setq proc-result (async-get proc))
    (when (file-exists-p logfile)
      (with-temp-buffer
	(insert-file-contents logfile)
	(setq log-text (buffer-string)))
      (delete-file logfile))
    (when (file-exists-p warnfile)
      (with-temp-buffer
	(insert-file-contents warnfile)
	(setq warn-text (buffer-string)))
      (delete-file warnfile))
    (when (file-exists-p msgfile)
      (with-temp-buffer
	(insert-file-contents msgfile)
	(setq msg-text (buffer-string)))
      (delete-file msgfile))
    (unboxed--with-snaps
     (log msgs warns)
     (setf (unboxed-installed-file-log inst) log)
     (setf (unboxed-installed-file-warnings inst) warns)
     (setf (unboxed-installed-file-messages inst) msgs))
    `((compiled ,(file-exists-p elc-path))
      (messages (host ,msgs) (sandbox ,msg-text))
      (warnings (host ,warns) (sandbox ,warn-text))
      (log (host ,log) (sandbox ,log-text)))))

(defun unboxed--async-byte-compile-library (db installed-file)
  "Byte-compile library file of INSTALLED-FILE for package in DB in \
asyncronous sandbox.
This function defines the following global symbols during compile, so \
a package may capture their value in an `eval-when-compile' form.
  `unboxed-package' Name of the package being installed as a symbol
  `unboxed-package-version' Version of the package being installed as \
  a string
  `unboxed-package-box' Directory containing the unpacked archive of \
  the package
  `unboxed-library-directory' Directory containing the top-level elisp \
  libraries of unboxed packages
  `unboxed-theme-directory' Directory containing theme files from \
  unboxed packages
  `unboxed-info-directory' Directory containing info files from \
  unboxed packages
  `unboxed-package-data-directory' Package-specific directory \
  containing any other installed files from this package."
  (when (unboxed-installed-file-created installed-file)
    (unboxed--start-snaps log msgs warns)
    (let ((area (unboxed--sexpr-db-area db))
	  (cats (unboxed--sexpr-db-categories db))
	  (pkg-name (unboxed-installed-file-package installed-file))
	  (pkg-version (unboxed-installed-file-package-version-string installed-file))
	  (pkg-loc (unboxed-installed-file-package-location installed-file))
	  (cat (unboxed-installed-file-category installed-file))
	  (cat-loc (unboxed-installed-file-category-location installed-file))
	  (src (unboxed-installed-file-package-source installed-file))
	  (lib (unboxed-installed-file-file installed-file))
	  (libdir (unboxed--sexpr-db-category-location db 'library))
	  (themedir (unboxed--sexpr-db-category-location db 'theme))
	  (infodir (unboxed--sexpr-db-category-location db 'info))
	  (datadir (unboxed--sexpr-db-category-location db 'data))
	  (elc-installed (unboxed-installed-file-struct-copy installed-file))
	  (autoloads (unboxed--scoped-autoloads db))
	  (lp-libdirs (unboxed--scoped-libdirs db))
	  logfile-base logfile log-text warn-text msg-text
	  el-name elc-name elc-path result warnfile msgfile
	  proc proc-result sys-lp lp load-sexprs)
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
	    program
	    `(lambda ()
	       (condition-case nil
		   (progn
		     (require 'bytecomp)
		     (setq load-path ',lp
			   unboxed-package ',pkg-name
			   unboxed-package-version ,pkg-version
			   unboxed-package-box ,pkg-loc
			   unboxed-library-directory ,libdir
			   unboxed-theme-directory ,themedir
			   unboxed-info-directory ,infodir
			   unboxed-package-data-directory ,datadir)
		     ;; ensure prompts during byte-compiling cause failure and do not just hang
		     (defun yes-or-no-p (prompt)
		       (error "Interactive yes-or-no-p prompting not allowed in batch compile mode - %S" prompt))
		     (defun y-or-n-p (prompt)
		       (error "Interactive y-or-n-p prompting not allowed in batch compile mode - %S" prompt))
		     (defun y-or-n-p-with-timeout (prompt seconds default)
		       (error "Interactive y-or-n-p-with-timeout prompting not allowed in batch compile mode - %S %S %S"
			      prompt seconds default))
		     (when (file-exists-p ,elc-path)
		       (delete-file ,elc-path))
		     ,@load-sexprs
		     (byte-compile-file ,el-path))
		 (error nil))
	       (let ((log-buffer (get-buffer byte-compile-log-buffer)))
		 (when log-buffer
		   (with-current-buffer log-buffer
		     (write-region nil nil ,logfile))))
	       (let ((log-buffer (get-buffer "*Warnings*")))
		 (when log-buffer
		   (with-current-buffer log-buffer
		     (write-region nil nil ,warnfile))))
	       (let ((log-buffer (get-buffer "*Messages*")))
		 (when log-buffer
		   (with-current-buffer log-buffer
		     (write-region nil nil ,msgfile))))))
      (setf (unboxed-installed-file-file elc-installed) elc-name)
      (setf (unboxed-installed-file-category elc-installed) 'byte-compiled)
      (setq proc (async-start program nil))
      (setq proc-result (async-get proc))
      (setf (unboxed-installed-file-created elc-installed)
	    (file-exists-p elc-path))
      (when (file-exists-p logfile)
	(with-temp-buffer
	  (insert-file-contents logfile)
	  (setq log-text (buffer-string)))
	(delete-file logfile))
      (when (file-exists-p warnfile)
	(with-temp-buffer
	  (insert-file-contents warnfile)
	  (setq warn-text (buffer-string)))
	(delete-file warnfile))
      (when (file-exists-p msgfile)
	(with-temp-buffer
	  (insert-file-contents msgfile)
	  (setq msg-text (buffer-string)))
	(delete-file msgfile))
      (unboxed--with-snaps
       (log msgs warns)
       (setq log-text (format "Host\n%s\nCompile Process\n%s" log log-text))
       (setq warn-text (format "Host\n%s\nCompile Process\n%s" warns warn-text))
       (setq msg-text (format "Host\n%s\nCompile Process\n%s" msgs msg-text)))
      (setf (unboxed-installed-file-log elc-installed) log-text)
      (setf (unboxed-installed-file-warnings elc-installed) warn-text)
      (setf (unboxed-installed-file-messages elc-installed) msg-text))
    '(,elc-installed)))


;;; install-action must take four arguments -
;;;  the package name as a symbol
;;;  the category name as a symbol
;;;  the source file name and
;;;  the location for installed files
;;; install-action returns a list of file names actually installed
;;; relative to the supplied location
(defun unboxed--install-list (cname db pd files install-action)
  "Install list of FILES of package PD in DB from category CNAME \
using INSTALL-ACTION."
  (let ((cat (cdr (assq cname (unboxed--sexpr-db-categories db))))
	(ls files)
	(pkg (unboxed-package-desc-name pd))
	(pkg-loc (unboxed-package-desc-dir pd))
	cat-loc
	installed
	file
	installed-files
	installed-file)
    (setq cat-loc (unboxed-file-category-location cat))
    (while ls
      (setq file (pop ls))
      (setq basename (file-name-nondirectory file)
	    relpath (file-name-directory file)
	    installed-file (funcall install-action
				     db
				     pd
				     cat
				     file))
      (when installed-file
	(setq installed-files (nconc installed-file installed-files))))
    (setq installed (nreverse installed-files))
    installed))

(defun unboxed--sexpr-rewriting-copy (src dest sexpr-pred)
  "Rewrite file SRC to DEST using SEXPR-PRED.".
  (with-temp-buffer
    (insert-file-contents src)
    (unboxed--pcase-replace-sexpr sexpr-pred)
    (write-region nil nil dest)))

(defun unboxed--simple-copy (src dest &optional aux)
  "Copy file SRC to DEST.  AUX data is ignored."
  (copy-file src dest t))

(defun unboxed--install-copy (db pd cat file copy-action &optional aux)
  "Install file into CAT location using COPY-ACTION and AUX data.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  CAT - category name
  FILE - file in package's boxed location
  COPY-ACTION - function with arguments (SOURCE DEST AUX)
  AUX - optional data to be passed to COPY-ACTION"
  (unboxed--start-snaps log msgs warns)
  (let ((version (unboxed-package-desc-version-string pd))
	(pkg (unboxed-package-desc-name pd))
	(cname (unboxed-file-category-name cat))
	(dst-loc (unboxed-file-category-location cat))
	(src-loc (unboxed-package-desc-dir pd))
	inst dest src dst-file)
    (setq dst-file (file-name-nondirectory file)
	  dest (file-name-concat dst-loc dst-file)
	  src (file-name-concat src-loc file))
    (funcall copy-action src dest aux)
    (setq inst
	  (unboxed-installed-file-create :package pkg
					 :package-version-string version
					 :package-location src-loc
					 :category cname
					 :category-location dst-loc
					 :file dst-file
					 :package-source file
					 :created (file-exists-p dest)))
    (unboxed--with-snaps
     (log msgs warns)
     (setf (unboxed-installed-file-log inst) log)
     (setf (unboxed-installed-file-warnings inst) warns)
     (setf (unboxed-installed-file-messages inst) msgs))
    `(,inst)))

(defun unboxed--install-rewriting-library-copy (db pd cat file)
  "Install file into CAT location with rewriting if needed.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  CAT - category name
  FILE - file in package's boxed location"
  (if (unboxed--contains-boxed-sexprs-p db pd cat file)
      (unboxed--install-copy
       db pd cat file
       #'unboxed--sexpr-rewriting-copy
       (unboxed--make-rewrite-boxed-sexprs db pd cat file))
    (unboxed--install-copy
     db pd cat file
     #'unboxed--simple-copy)))

(defun unboxed--install-simple-copy (db pd cat file)
  "Copies file into CAT location.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  CAT - category name
  FILE - file in package's boxed location"
  (unboxed--install-copy db pd cat file #'unboxed--simple-copy))

(defun unboxed--install-pkg-relative-copy (db pd cat file)
  "Install file in package-specific subdirectory of CAT.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  CAT - category name
  FILE - file in package's boxed location"
  (let ((version (unboxed-package-desc-version-string pd))
	(pkg (unboxed-package-desc-name pd))
	(cname (unboxed-file-category-name cat))
	(dst-loc (unboxed-file-category-location cat))
	(src-loc (unboxed-package-desc-dir pd))
	inst dest src dst-file)
    (setq dst-file (file-name-concat (symbol-name pkg)
				     file)
	  dest (file-name-concat dst-loc dst-file)
	  src (file-name-concat src-loc file))
    (when (> (length (file-name-nondirectory file)) 0)
      (make-directory (file-name-directory dest) t))
    (copy-file src dest t)
    (setq inst
	  (unboxed-installed-file-create :package pkg
					 :package-version-string version
					 :package-location src-loc
					 :category cname
					 :category-location dst-loc
					 :file dst-file
					 :package-source file))
    `(,inst)))


;;; "files" here are installed-file structs
;;; remove-action takes the same arguments as an install-cation
(defun unboxed--remove-list (db files remove-action)
  "Remove list of files FILES from DB using REMOVE-ACTION function.
Arguments:
  DB - unboxed database
  FILES - installed-files in DB area
  REMOVE-ACTION - function with arguments
     (PD CAT FILE BOXED-LOCATION UNBOXED-LOCATION)"
  (let ((ls files)
	cat
	cat-loc
	cname
	pd pkg pkg-loc
	deleted
	file
	removed-file)
    (while ls
      (setq installed-file (pop ls))
      (setq pkg (unboxed-installed-file-package installed-file))
      (setq pd (assoc pkg pkgs))
      (setq pd (and pd (cdr pd)))
      (unless pd
	(signal 'unboxed-invalid-package `(,pkg ,pkgs)))
      (setq pkg-loc (unboxed-package-desc-dir pd))
      (setq cname (unboxed-installed-file-category cat))
      (setq cat (assoc cname cats))
      (setq cat (and cat (cdr cat)))
      (unless cat
	(signal 'unboxed-invalid-category `(,cname ,cats)))
      (setq cat-loc (unboxed-file-category-location cat))
      (setq removed-file
	    (funcall remove-action
		     pkg
		     cname
		     (unboxed-installed-file-file installed-file)
		     pkg-loc
		     cat-loc))
      (setq deleted `(,@removed-file ,@deleted)))
    deleted))

(defun unboxed--remove-simple-delete (pkg cat file src-loc dst-loc)
  "Delete FILE of category CAT installed in DST-LOC from package PKG.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  CAT - category name
  FILE - file in package's boxed location"
  (let ((dest (file-name-concat dst-loc (file-name-non-directory file)))
	(src (file-name-concat src-loc file)))
    (condition-case nil
	(progn
	  (and (file-exists-p dest)
	       (delete-file dest))
	  `(,installed-file))
      (error ;; do nothing for now
	 nil))))


;;; These installers are used for lists of files of a particular
;;; category for a specific package
;;; file names are given as relative paths to the package directory
(defun unboxed-install-theme (db pd files)
  "Install theme files FILES for package PD of DB.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  FILES - file paths relateive to boxed directory of PD"
  (unboxed--install-list 'theme db pd files #'unboxed--install-simple-copy))
  
(defun unboxed-install-library (db pd files)
  "Install library files FILES for package PD of DB.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  FILES - file paths relateive to boxed directory of PD"
  (unboxed--install-list 'library db pd files #'unboxed--install-rewriting-library-copy))

(defun unboxed-install-module (db pd files)
  "Install module (shared library) files FILES for package PD of DB.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  FILES - file paths relateive to boxed directory of PD"
  (unboxed--install-list 'module db pd files #'unboxed--install-simple-copy))

(defun unboxed-install-info (db pd files)
  "Install info files FILES for package PD of DB.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  FILES - file paths relateive to boxed directory of PD"
  (unboxed--install-list 'info db pd files #'unboxed--install-simple-copy))

(defun unboxed-install-byte-compiled (db pd files)
  "Install (discard) byte-compiled files FILES for package PD of DB.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  FILES - file paths relateive to boxed directory of PD"
  nil)

(defun unboxed-install-native-compiled (db pd files)
  "Install (discard) byte-compiled files FILES for package PD of DB.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  FILES - file paths relateive to boxed directory of PD"
  nil)

(defun unboxed-install-data (db pd files)
  "Install residual files FILES for package PD of DB in package data directory.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  FILES - file paths relateive to boxed directory of PD"
  (let ((area (unboxed--sexpr-db-area db))
	(loc (unboxed-file-category-location
	      (cdr (assoc 'data
			  (unboxed--sexpr-db-categories db)))))
	(pkg (unboxed-package-desc-name pd))
	pkg-dir libs installed elcs)
    (setq pkg-dir (file-name-concat loc (symbol-name pkg)))
    (make-directory pkg-dir t)
    (setq installed (unboxed--install-list
		     'data db pd files
		     #'unboxed--install-pkg-relative-copy)
	  libs (seq-filter
		(lambda (inst)
		  (unboxed-data-library-p
		   (unboxed-installed-file-file inst)))
		installed)
	  elcs (mapcan
		(lambda (inst)
		  (unboxed--async-byte-compile-library db inst))
		libs))
    (nconc installed (nreverse elcs))))


;;; These removers are used for lists of installed files
(defun unboxed-remove-theme (db pd files)
  "Remove theme files FILES for package PD of DB.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  FILES - installed files from PD"
  (unboxed--remove-list db files #'unboxed--remove-simple-delete))
  
(defun unboxed-remove-library (db pd files)
  "Remove library files FILES for package PD of DB.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  FILES - installed files from PD"
  (unboxed--remove-list db files #'unboxed--remove-simple-delete))

(defun unboxed-remove-byte-compiled (db pd files)
  "Remove byte-compiled files FILES for package PD of DB.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  FILES - installed files from PD"
  nil)

(defun unboxed-remove-native-compiled (db pd files)
  "Remove native-compiled files FILES for package PD of DB.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  FILES - installed files from PD"
  nil)

(defun unboxed-remove-module (db pd files)
  "Remove module (shared library) files FILES for package PD of DB.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  FILES - installed files from PD"
  (unboxed--remove-list db files #'unboxed--remove-simple-delete))

(defun unboxed-remove-info (db pd files)
  "Remove info files FILES for package PD of DB.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  FILES - installed files from PD"
  (unboxed--remove-list db files #'unboxed--remove-simple-delete))

(defun unboxed-remove-data (db pd files)
  "Remove data files FILES for package PD of DB.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  FILES - installed files from PD"
  (unboxed--remove-list db files #'unboxed--remove-simple-delete))



;;; these functions are run with all the installed-file structs
;;; produced by a set of packages, after the above installers
;;; have completed for *all* the packages in that set.
;;; Return installed-file structs for any additional files produced
;;; during finalization that must be removed when uninstalling a
;;; package, e.g. elc files
;;;   all-cats is provided since the install locations vary between
;;; system and user package sets.

;; rebuild the unboxed library autoloads and byte-compile
;; the libraries
(defun unboxed-finalize-install-library (db cat files)
  "Finalize installation of library files FILES in category CAT of DB."
  "Test whether FILE contains a sexp referencing the package's location.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  CAT - category name
  FILES - files in the category's location"
  (let ((loc (unboxed-file-category-location cat))
	(area (unboxed--sexpr-db-area db))
	autoloads-fn autoloads-file result ls
	inst comp-file new-installed)
    (setq autoloads-fn (unboxed--area-autoloads-file area)
	  autoloads-file (expand-file-name (file-name-concat loc autoloads-fn))
	  ls files)
    (let ((al-buffer (get-file-buffer autoloads-file)))
      (when al-buffer
	(with-current-buffer al-buffer
	  (set-buffer-modified-p nil))
	(kill-buffer al-buffer)))
    (make-directory-autoloads loc autoloads-file)
    (let ((default-directory loc))
      (setq result (unboxed--async-byte-compile-file autoloads-file))
      (when (and (stringp result) (> (length result) 0))
	(message "Compile log for %S\n%s" autoloads-file result))
      (while ls
	(setq inst (pop ls))
	(setq comp-file (unboxed--async-byte-compile-library db inst))
	(when comp-file
	  (setq new-installed (nconc comp-file new-installed)))))
    new-installed))

(defun unboxed-finalize-install-byte-compiled (db cat files)
  "Finalize installation of byte-compiled files FILES in category CAT of DB."
  "Test whether FILE contains a sexp referencing the package's location.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  CAT - category name
  FILES - files in the category's location"
  nil)

(defun unboxed-finalize-install-native-compiled (db cat files)
  "Finalize installation of native-compiled files FILES in category CAT of DB."
  "Test whether FILE contains a sexp referencing the package's location.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  CAT - category name
  FILES - files in the category's location"
  nil)

;; rebuild the directory file
(defun unboxed-finalize-install-info (db cat files)
  "Finalize installation of info files FILES in category CAT of DB."
  "Test whether FILE contains a sexp referencing the package's location.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  CAT - category name
  FILES - files in the category's location"
  (let ((ls files)
	file)
    (while ls
      (setq file (pop ls))
      (unboxed--install-info-file-in-dir file))
    files))

;; other categories require no additional work
(defun unboxed-finalize-install-module (db cat files)
  "Finalize installation of module files FILES in category CAT of DB."
  "Test whether FILE contains a sexp referencing the package's location.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  CAT - category name
  FILES - files in the category's location"
  nil)

(defun unboxed-finalize-install-data (db cat files)
  "Finalize installation of data files FILES in category CAT of DB."
  "Test whether FILE contains a sexp referencing the package's location.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  CAT - category name
  FILES - files in the category's location"
  nil)

(defun unboxed-finalize-install-theme (db cat files)
  "Finalize installation of theme files FILES in category CAT of DB."
  "Test whether FILE contains a sexp referencing the package's location.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  CAT - category name
  FILES - files in the category's location"
  nil)

(defun unboxed-finalize-remove-theme (db cat files)
  "Finalize removal of theme files FILES in category CAT of DB."
  "Test whether FILE contains a sexp referencing the package's location.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  CAT - category name
  FILES - files in the category's location"
  nil)
  
(defun unboxed-finalize-remove-library (db cat files)
  "Finalize removal of library files FILES in category CAT of DB."
  "Test whether FILE contains a sexp referencing the package's location.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  CAT - category name
  FILES - files in the category's location"
  nil)

(defun unboxed-finalize-remove-byte-compiled (db cat files)
  "Finalize removal of byte-compiled files FILES in category CAT of DB."
  "Test whether FILE contains a sexp referencing the package's location.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  CAT - category name
  FILES - files in the category's location"
  nil)

(defun unboxed-finalize-remove-native-compiled (db cat files)
  "Finalize removal of native-compiled files FILES in category CAT of DB."
  "Test whether FILE contains a sexp referencing the package's location.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  CAT - category name
  FILES - files in the category's location"
  nil)

(defun unboxed-finalize-remove-module (db cat files)
  "Finalize removal of module files FILES in category CAT of DB."
  "Test whether FILE contains a sexp referencing the package's location.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  CAT - category name
  FILES - files in the category's location"
  nil)

(defun unboxed-finalize-remove-info (db cat files)
  "Finalize removal of info files FILES in category CAT of DB."
  "Test whether FILE contains a sexp referencing the package's location.
Arguments:
  DB - unboxed database
  PD - unboxed package descriptor
  CAT - category name
  FILES - files in the category's location"
  nil)

(defun unboxed-finalize-remove-data (db cat files)
  "Finalize removal of data files FILES in category CAT of DB."
  "Test whether FILE contains a sexp referencing the package's location.
Arguments:
  DB - unboxed database
  CAT - category name
  FILES - files in the category's location"
  nil)

	


(provide 'unboxed-file-management)

;;; unboxed-file-management.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("ajq-" . "async-job-queue-")("ub-" . "unboxed-"))
;; End:
