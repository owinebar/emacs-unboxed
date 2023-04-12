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
(require 'unboxed-decls)
(require 'unboxed-categories)
(require 'unboxed-rewrite-sexprs)

(defun unboxed--file-grep (re file)
  (with-temp-buffer
    (insert-file-contents file)
    (string-match-p re (buffer-string))))

   
(defun unboxed--install-info-file-in-dir (installed-file)
  "Utility for creating entry for an unboxed package info file in the dir file
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
  "Utility for creating entry for an unboxed package info file in the dir file
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
  ;; (unless unboxed-temp-directory
  ;;   (setq unboxed-temp-directory (file-name-concat user-emacs-directory
  ;; 						   "tmp")))
  ;; (setq unboxed-temp-directory
  ;; 	(file-name-as-directory unboxed-temp-directory))
  ;; (unless (file-accessible-directory-p unboxed-temp-directory)
  ;;   (make-directory unboxed-temp-directory t))
  (let ((logfile-base (concat base
			      "-"
			      (symbol-name pkg-name)
			      (if filename
				  (concat "--"
					  (file-name-nondirectory filename))
				"")
			      "-"))
	(temporary-file-directory unboxed-temp-directory))
    (make-temp-file logfile-base)))

(defun unboxed--async-byte-compile-file (file)
  "Function to byte compile an elisp file"
  (let ((el-name (file-name-nondirectory file))
	logfile-base 
	logfile
	log-text
	elc-name
	elc-path
	result
	proc
	proc-result)
    (setq logfile-base (file-name-sans-extension el-name)
	  logfile (let ((temporary-file-directory unboxed-temp-directory))
		    (make-temp-file 
		     (concat "compile-log--" logfile-base "-")))
	  el-path (expand-file-name el-name)
	  elc-path  (if (string= (file-name-extension el-path) "el")
			(concat el-path "c")
		      (concat el-path ".elc"))
	  program
	  `(lambda ()
	     (require 'bytecomp)
	     (setq load-path ',load-path)
	     (defvar logtext nil)
	     (when (file-exists-p ,elc-path)
	       (delete-file ,elc-path))
	     (byte-compile-file ,el-path)
	     (let ((log-buffer (get-buffer byte-compile-log-buffer)))
	       (when log-buffer
		 (with-current-buffer log-buffer
		   (setq logtext (buffer-string))
		   (write-region (point-min) (point-max) ,logfile))))
	     (if (> (length logtext) 0)
		 logtext
	       t)))
    (setq proc (async-start program nil))
    (setq proc-result (async-get proc))
    (with-temp-buffer
      (insert-file-contents logfile)
      (setq log-text (buffer-string)))
    (when (file-exists-p elc-path)
      (setq result log-text))
    (when (file-exists-p logfile)
      (delete-file logfile))
    result))

(defun unboxed--async-byte-compile-library (db installed-file)
  "Function to byte compile an unboxed elisp library from a package.
This function defines the following global symbols during compile, so
a package may capture their value in an eval-when-compile form.
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
  containing any other installed files from this package."
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
	logfile-base logfile log-text el-name elc-name elc-path result
	proc proc-result)
    (setq logfile (unboxed--make-install-logfile "compile-log" pkg-name lib)
	  datadir (file-name-concat datadir (symbol-name pkg-name))
	  el-name lib
	  elc-name  (if (string= (file-name-extension el-name) "el")
			(concat el-name "c")
		      (concat el-name ".elc"))
	  el-path (file-name-concat cat-loc el-name)
	  elc-path (file-name-concat cat-loc elc-name)
	  program
	  `(lambda ()
	     (require 'bytecomp)
	     (setq load-path ',load-path)
	     (setq unboxed-package ',pkg-name)
	     (setq unboxed-package-version ,pkg-version)
	     (setq unboxed-package-box ,pkg-loc)
	     (setq unboxed-library-directory ,libdir)
	     (setq unboxed-theme-directory ,themedir)
	     (setq unboxed-info-directory ,infodir)
	     (setq unboxed-package-data-directory ,datadir)
	     (setq elc-name ,elc-name)
	     (setq cat-loc ,cat-loc)
	     (setq el-path ,el-path)
	     (setq elc-path ,elc-path)
	     (when (file-exists-p elc-path)
	       (delete-file elc-path))
	     (byte-compile-file el-path)
	     (let ((log-buffer (get-buffer byte-compile-log-buffer)))
	       (when log-buffer
		 (with-current-buffer log-buffer
		   (write-region (point-min) (point-max) logfile))))))
    (setf (unboxed-installed-file-file elc-installed) elc-name)
    (setf (unboxed-installed-file-category elc-installed) 'byte-compiled)
    (setq proc (async-start program nil))
    (setq proc-result (async-get proc))
    (with-temp-buffer
      (insert-file-contents logfile)
      (setq log-text (buffer-string)))
    (when (file-exists-p elc-path)
      (setf (unboxed-installed-file-log elc-installed) log-text)
      (setq result `(,elc-installed)))
    (when (file-exists-p logfile)
      (delete-file logfile))
    result))

;;; install-action must take four arguments -
;;;  the package name as a symbol
;;;  the category name as a symbol
;;;  the source file name and
;;;  the location for installed files
;;; install-action returns a list of file names actually installed
;;; relative to the supplied location
(defun unboxed--install-list (cname db pd files install-action)
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
  (with-temp-buffer
    (insert-file-contents src)
    (unboxed--pcase-replace-sexpr sexpr-pred)
    (write-region nil nil dest)))

(defun unboxed--simple-copy (src dest &optional aux)
  (copy-file src dest t))

(defun unboxed--install-copy (db pd cat file copy-action &optional aux)
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
					 :package-source file))
    `(,inst)))

(defun unboxed--install-rewriting-library-copy (db pd cat file)
  ;; FIXME - predicate should be configurable
  (if (and (not (unboxed-package-desc-simple pd))
	   (unboxed--file-grep
	    "load-file-name"
	    (expand-file-name file (unboxed-package-desc-dir pd))))
      (let ((sym (unboxed--sexpr-db-datadir-patterns db)))
	(unboxed--install-copy
	 db pd cat file
	 #'unboxed--sexpr-rewriting-copy
	 (get sym 'unboxed-rewriter)))
    (unboxed--install-copy
     db pd cat file
     #'unboxed--simple-copy)))

(defun unboxed--install-simple-copy (db pd cat file)
  (unboxed--install-copy db pd cat file
			 (lambda (src dest)
			   (copy-file src dest t))))

(defun unboxed--install-pkg-relative-copy (db pd cat file)
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
(defun unboxed--remove-list (area files remove-action)
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
  (unboxed--install-list 'theme db pd files #'unboxed--install-simple-copy))
  
(defun unboxed-install-library (db pd files)
  (unboxed--install-list 'library db pd files #'unboxed--install-rewriting-library-copy))

(defun unboxed-install-module (db pd files)
  (unboxed--install-list 'module db pd files #'unboxed--install-simple-copy))

(defun unboxed-install-info (db pd files)
  (unboxed--install-list 'info db pd files #'unboxed--install-simple-copy))

(defun unboxed-install-byte-compiled (db pd files)
  nil)

(defun unboxed-install-native-compiled (db pd files)
  nil)

(defun unboxed-install-data (db pd files)
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
(defun unboxed-remove-theme (area files)
  (unboxed--remove-list area files #'unboxed--remove-simple-delete))
  
(defun unboxed-remove-library (area files)
  (unboxed--remove-list area files #'unboxed--remove-simple-delete))

(defun unboxed-remove-byte-compiled (area pd files)
  nil)

(defun unboxed-remove-native-compiled (area pd files)
  nil)

(defun unboxed-remove-module (area files)
  (unboxed--remove-list area files #'unboxed--remove-simple-delete))

(defun unboxed-remove-info (area files)
  (unboxed--remove-list area files #'unboxed--remove-simple-delete))

(defun unboxed-remove-data (area files)
  (unboxed--remove-list area files #'unboxed--remove-simple-delete))



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

(defun unboxed-finalize-install-byte-compiled (area cat files)
  nil)

(defun unboxed-finalize-install-native-compiled (area cat files)
  nil)

;; rebuild the directory file
(defun unboxed-finalize-install-info (db cat files)
  (let ((ls files)
	file)
    (while ls
      (setq file (pop ls))
      (unboxed--install-info-file-in-dir file))
    files))

;; other categories require no additional work
(defun unboxed-finalize-install-module (db cat files)
  nil)

(defun unboxed-finalize-install-data (db cat files)
  nil)

(defun unboxed-finalize-install-theme (db cat files)
  nil)

(defun unboxed-finalize-remove-theme (db cat files)
  nil)
  
(defun unboxed-finalize-remove-library (db cat files)
  nil)

(defun unboxed-finalize-remove-byte-compiled (area pd files)
  nil)

(defun unboxed-finalize-remove-native-compiled (area pd files)
  nil)

(defun unboxed-finalize-remove-module (db cat files)
  nil)

(defun unboxed-finalize-remove-info (db cat files)
  nil)

(defun unboxed-finalize-remove-data (db cat files)
  nil)

	
(provide 'unboxed-file-management)

;;; unboxed-file-managment.el ends here

;; 
