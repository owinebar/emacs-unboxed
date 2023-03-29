;;; unboxed.el --- Unboxed package management        -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Onnie Winebarger

;; Author: Onnie Winebarger;; Copyright (C) 2023 by Onnie Lynn Winebarger <owinebar@rapscallion>
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

;; Unboxed installs packages in a single "packages" directory whenever
;; possible.
;;
;; The update of autoloads and byte-compiling steps for elisp libraries are
;; performed once per set of packages, rather than recompiling the
;; quickstart file for each individual package installed.  When a
;; large number of packages are installed, this is much more
;; efficient.
;;
;; Theme and info files are installed in dedicated locations rather
;; than growing the associated path variable.

;;; Code:

(require 'package)
(require 'cl-lib)
(require 'seq)
(require 'async)
(require 'unboxed-decls)
(require 'unboxed-categories)
(require 'unboxed-file-management)
(require 'unboxed-custom)
(require 'unboxed-rewrite-sexprs)
    
(define-error 'unboxed-invalid-package
  "Unrecognized package name")
(define-error 'unboxed-invalid-category
  "Unrecognized category name")
(define-error 'unboxed-invalid-category-spec
  "One or more fields in a file category specification is invalid")

(defvar unboxed--packages-to-install
  "List of packages to be unboxed once they are available as ordinary
packages in the package directory"
  nil)

(defvar unboxed--packages-to-remove
  "List of unboxed packages to be removed before removing the ordinary
packages in the package directory"
  nil)

(defvar unboxed--packages-to-rebox
  "List of unboxed packages to be put back under the standard package
installation management"
  nil)

(defvar unboxed--areas nil)

(defun unboxed--is-user-package (pd)
  (let ((d (package-desc-dir pd)))
    (and d
	 (stringp d)
	 (string-prefix-p package-user-dir d))))

(defun unboxed--is-site-package (pd)
  (let ((d (package-desc-dir pd)))
    (and d
	 (stringp d)
	 (seq-filter (lambda (p) (string-prefix-p p d))
		     package-directory-list))))


(defun unboxed--init-package-desc (mgr pd)
  "Initialize an unboxed package descriptor from the manager field and a pre-existing package-desc"
  (let ((s (unboxed--package-desc-create :manager 'package))
	n)
    (setq n (length pd))
    (if (recordp s)
	(let ((i 1))
	  (while (< i n)
	    (aset s i (aref pd i))
	    (cl-incf i)))
      (let ((i0 1))
	(unless (eq (seq-elt s 0) 'unboxed--package-desc)
	  (setq i0 0))
	(seq-map-indexed (lambda (elt idx)
			   (when (>= idx i0)
			     (setf (seq-elt s idx) elt)))
			 pd)))))


(defun unboxed--create-sexpr-db (type)
  "Create an unboxed db in the sexpr format - initialize from package-desc table assuming no packages have been unboxed"
  (let (cats pdlo pkgs inst inst-by-cat)
    (setq cats
	  (mapcar #'(lambda (setting)
		      (let ((file-cat (apply #'unboxed--categories-setting->struct setting)))
			`(,(unboxed--file-category-name file-cat) . ,file-cat)))
		  (if (eq type 'system)
		      unboxed-site-categories
		    unboxed-user-categories)))
    (setq pdlo
	  (unboxed--package-desc-layout-create
	   (cl-struct-sequence-type 'package-desc)
	   (cl-struct-slot-info 'pacakge-desc)))
    (setq pkgs (make-hash-table :test #'eq))
    (mapc (lambda (pd)
	    (let ((upd (unboxed--init-package-desc 'package pd)))
	      (puthash pkgs (unboxed--package-desc-name upd) upd)))
	  (seq-filter (if (eq type 'system)
			  #'unboxed--is-site-package
			#'unboxed--is-user-package)
		      package-alist))
    ;; these record installed files managed by unboxed, so they start out empty
    (setq inst (make-hash-table :test #'equal))
    (setq inst-by-cat (make-hash-table :test #'eq))
    (unboxed--sexpr-db-create
     :categories cats
     :package-desc-layout pdlo
     :packages pkgs
     :installed inst
     :installed-by-cat inst-by-cat)))

(defcustom unboxed-data-directory-patterns
  '()
  "A list of pcase patterns that match against known expressions used to compute a packages installation directory
at either compile or load time.  Any matches will be hard-coded to be the value
of the unboxed package's data directory as a string."
  :type '(repeat (sexpr :tag "pcase pattern"))
  :group unboxed
  :setter #'unboxed--set-pcase-replace-sexpr-p
  )

    
(provide 'unboxed)
;;; unboxed.el ends here

;; 

