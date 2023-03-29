;;; unboxed-decls.el        -*- lexical-binding: t; -*-

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

;; Functions for implementing file-category definitions

;;; Code:

;;; libraries to install in the package lisp directory
(defun unboxed-library-p (path)
  "Predicate for elisp libraries contained in packages that should be
installed in the unboxed library directory. This predicate only
recognizes files matching `*.el' in top directory of the package
archive, and excludes `*-pkg.el' and `*-autoloads.el' files, since the
former are not proper elisp and the latter are not useful in an
unboxed installation." 
  (let ((ext (file-name-extension path))
	(base (file-name-nondirectory path))
	(dir (file-name-directory path)))
    (and ext (string= ext "el")
	 (not dir)
	 (or (not (string-suffix-p "-theme" base))
	     (memq (intern base) unboxed-theme-libraries))
	 (not (string-suffix-p "-package" base))
	 (not (string-suffix-p "-autoloads" base)))))

;;; modules to install in the package lisp directory
(defun unboxed-module-p (path)
  "Predicate for elisp modules contained in packages that should be
installed in the unboxed library directory." 
  (let ((ext (file-name-extension path))
	(base (file-name-nondirectory path))
	(dir (file-name-directory path)))
    (and ext (string= ext "so"))))

;;; theme files
(defun unboxed-theme-p (path)
  "Predicate for elisp libraries contained in packages that should be
installed in the unboxed theme directory. Any `*-theme.el' file whose
feature name is not contained in unboxed-theme-libraries variable is
classified as a theme."
  (let ((ext (file-name-extension path))
	(base (file-name-nondirectory path))
	(dir (file-name-directory path)))
    (and ext (string= ext "el")
	 (string-suffix-p "-theme" base)
	 (not (memq (intern base) unboxed-theme-libraries)))))

;;; info files
(defun unboxed-info-p (path)
  "Predicate for files contained in packages that should be installed
in the unboxed info directory. This predicate recognizes all `*.info'
and 'dir' files as info files"
  (let ((ext (file-name-extension path)))
    (or (and ext (string= ext "info"))
	(string= (file-name-nondirectory path) "dir"))))

;;; files to ignore
(defun unboxed-compiled-elisp-p (path)
  "Predicate for files contained in packages that should be installed
in the unboxed info directory. This predicate recognizes all `*.info'
and 'dir' files as info files"
  (let ((ext (file-name-extension path)))
    (and ext (string= ext "elc"))))


;;; Anything else
(defun unboxed-data-p (path)
  "Predicate for files contained in packages that should be installed
in the unboxed data directory for the package,
`<unboxed-data-directory>/<package-name>'.  This predicate is expected
to be executed last and just returns true."
  t)

(defun unboxed--categories-setting->struct (cat pred-name dir-var
						install-name
						finalize-install-name
						remove-name
						finalize-remove-name)
  (let ((pred (and (fboundp pred-name) (symbol-function pred-name)))
	(dir (and (boundp dir-var) (symbol-value dir-var)))
	(install (and (fboundp install-name) (symbol-function install-name)))
	(finalize-install (and (fboundp finalize-install-name)
						(symbol-function
						finalize-install-name)))
	(remove (and (fboundp remove-name) (symbol-function remove-name)))
	(finalize-remove (and (fboundp finalize-remove-name)
			      (symbol-function finalize-remove-name))))
    (unless (and pred dir install)
      (signal 'unboxed-invalid-category-spec
	      `(,cat (,pred-name ,pred)
		     (,dir-var ,dir)
		     (,install-name ,install)
		     (,finalize-install-name ,finalize-install)
		     (,remove-name ,remove)
		     (,finalize-remove-name ,finalize-remove))))
    (unboxed--file-category-create
     :name cat
     :predicate pred
     :location dir
     :install-files install
     :finalize-install-files finalize-install
     :remove-files remove
     :finalize-remove-files finalize-remove)))


(provide 'unboxed-categories)

;;; unboxed-decls.el ends here

;; 
