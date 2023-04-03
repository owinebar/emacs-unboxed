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
    (setf (unboxed-installed-file-log installed-file) log-text)))

(defun unboxed--async-byte-compile-library (cat area pd installed-file)
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
  (let ((pkg-name (unboxed-installed-file-package installed-file))
	(pkg-version (unboxed-installed-file-package-version installed-file))
	(pkg-loc (unboxed-installed-file-package-location
		  installed-file))
	(cat (unboxed-installed-file-category installed-file))
	(cat-loc (unboxed-installed-file-category-location
		  installed-file))
	(src (unboxed-installed-file-package-source installed-file))
	(lib (unboxed-installed-file-file installed-file))
	(libdir (unboxed-file-category-location
		 (cdr (assq 'library cats))))
	(themedir (unboxed-file-category-location
		 (cdr (assq 'theme cats))))
	(infodir (unboxed-file-category-location
		  (cdr (assq 'info cats))))
	(datadir  (unboxed-file-category-location
		  (cdr (assq 'data cats))))
	(elc-installed (unboxed-installed-file-struct-copy
			installed-file))
	(logfile
	 (let ((temporary-file-directory unboxed-temp-directory))
	   (make-temp-file (concat "compile-log-"
				   (symbol-name pkg-name)
				   "--"
				   (file-name-nondirectory lib)
				   "-"))))
	log-text el-name elc-name)
    (setq datadir (file-name-concat datadir (symbol-name pkg-name)))
    (async-start
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
	(byte-compile-file ,(file-name-concat cat-loc lib))
	(let ((log-buffer (get-buffer byte-compile-log-buffer)))
	  (when log-buffer
	    (with-current-buffer log-buffer
	      (write-region (point-min) (point-max) logfile)))))
     (lambda (&optional dummy)
       (with-temp-buffer
	 (insert-file-contents logfile)
	 (setq log-text (buffer-string)))
       (setq el-name lib)
       (setq elc-name
	     ;; just in case
	     (if (string= (file-name-extension el-name) "el")
		 (concat el-name "c")
	       (concat el-name ".elc")))
       (setf (unboxed-installed-file-file elc-installed) elc-name)
       (setf (unboxed-installed-file-log elc-installed) log-text)
       `(,elc-installed)))))

;;; install-action must take four arguments -
;;;  the package name as a symbol
;;;  the category name as a symbol
;;;  the source file name and
;;;  the location for installed files
;;; install-action returns a list of file names actually installed
;;; relative to the supplied location
(defun unboxed--install-list (area cat pd files install-action)
  (let ((ls files)
	(cat-loc (unboxed-file-category-location cat))
	(cname (unboxed-file-category-name cat))
	(pkg (unboxed-package-desc-name pd))
	(pkg-loc (unboxed-package-desc-dir pd))
	installed
	file
	installed-files
	installed-file)
    (while ls
      (setq file (pop ls))
      (setq basename (file-name-nondirectory file)
	    relpath (file-name-directory file))
      (setq installed-files
	    (funcall install-action
		     (file-name-concat pkg-loc file)
		     cat-loc))
      (while installed-files
	(setq installed-file (pop installed-files))
	(push (unboxed-installed-file-create :package pkg
					     :category cname
					     :file installed-file
					     :pkg-path file)
	      installed)))
    installed))

(defun unboxed--install-simple-copy (area pkg cat file src-loc dst-loc)
  (let ((dest (file-name-concat dst-loc (file-name-non-directory file)))
	(src (file-name-concat src-loc file)))
    (copy-file src dest t)
    `(,dest)))

(defun unboxed--install-pkg-relative-copy (area pkg cat file src-loc dst-loc)
  (let ((dest (file-name-concat dst-loc pkg file))
	(src (file-name-concat src-loc file)))
    (when (> (length (file-name-nondirectory file)) 0)
      (make-directory (file-name-directory dest) t))
    (copy-file src dest t)
    `(,dest)))


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
(defun unboxed-install-theme (area pd files)
  (unboxed--install-list 'theme area pd #'unboxed--install-simple-copy))
  
(defun unboxed-install-library (area pd files)
  (unboxed--install-list 'library area pd #'unboxed--install-simple-copy))

(defun unboxed-install-module (area pd files)
  (unboxed--install-list 'module area pd #'unboxed--install-simple-copy))
(defun unboxed-install-info (area pd files)
  (unboxed--install-list 'info area pd files #'unboxed--install-simple-copy))
(defun unboxed-install-data (area pd files)
  (let ((loc (unboxed-file-category-location
	      (cdr (assoc 'data
			  (unboxed--area-categories area)))))
	(pkg (unboxed-package-desc-name pd)))
    (make-directory (file-name-concat loc pkg) t))
  (unboxed--install-list 'data area files #'unboxed--install-pkg-relative-copy))

(defun unboxed-install-theme (area pd files)
  (unboxed--install-list area pd files #'unboxed--install-simple-copy))

;;; These removers are used for lists of installed files
(defun unboxed-remove-theme (area files)
  (unboxed--remove-list area files #'unboxed--remove-simple-delete))
  
(defun unboxed-remove-library (area files)
  (unboxed--remove-list area files #'unboxed--remove-simple-delete))

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
(defun unboxed-finalize-install-library (all-cats cat files)
  )

;; rebuild the directory file
(defun unboxed-finalize-install-info (all-cats cat files)
  )

;; other categories require no additional work
(defun unboxed-finalize-install-module (all-pkgs all-cats cat files)
  nil)
(defun unboxed-finalize-install-data (all-pkgs all-cats cat files)
  nil)
(defun unboxed-finalize-install-theme (all-pkgs all-cats cat files)
  nil)

(defun unboxed-finalize-remove-theme (all-pkgs all-cats files)
  nil)
  
(defun unboxed-finalize-remove-library (all-pkgs all-cats files)
  nil)

(defun unboxed-finalize-remove-module (all-pkgs all-cats files)
  nil)

(defun unboxed-finalize-remove-info (all-pkgs all-cats files)
  nil)

(defun unboxed-finalize-remove-data (all-pkgs all-cats files)
  nil)

(defun unboxed--install-package-files (area pd)
  "Utility function to run the first-stage unboxing of each file in a
package that belongs to some category for its area"
  (let ((d (package-desc-dir pd))
	(cats (unboxed--area-categories area)))))

	
(provide 'unboxed-file-management)

;;; unboxed-file-managment.el ends here

;; 
