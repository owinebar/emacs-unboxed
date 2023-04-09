;;; unboxed-categories.el        -*- lexical-binding: t; -*-

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

;; Functions for implementing file-category definitions

;;; Code:

(require 'unboxed-decls)

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

(defun unboxed-data-library-p (path)
  "Predicate for elisp libraries contained in package data directorys
that should be compiled there." 
  (let ((ext (file-name-extension path))
	(base (file-name-nondirectory path))
	(dir (file-name-directory path)))
    (and ext (string= ext "el")
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
    (and ext
	 (or (string= ext "elc")
	     (string= ext "eln")))))

(defun unboxed-byte-compiled-p (path)
  "Predicate for files contained in packages that should be installed
in the unboxed info directory. This predicate recognizes all `*.info'
and 'dir' files as info files"
  (let ((ext (file-name-extension path)))
    (and ext (string= ext "elc"))))

(defun unboxed-native-compiled-p (path)
  "Predicate for files contained in packages that should be installed
in the unboxed info directory. This predicate recognizes all `*.info'
and 'dir' files as info files"
  (let ((ext (file-name-extension path)))
    (and ext (string= ext "eln"))))


;;; Anything else
(defun unboxed-data-p (path)
  "Predicate for files contained in packages that should be installed
in the unboxed data directory for the package,
`<unboxed-data-directory>/<package-name>'.  This predicate is expected
to be executed last and just returns true."
  t)

(defun unboxed--function-or-nil (val)
  (cond
   ((symbolp val)
    (and (fboundp val) (symbol-function val)))
   (t val)))

(defun unboxed--string-or-nil (val)
  (cond
   ((symbolp val)
    (and (boundp val) (symbol-value val)))
   (t val)))

(defun unboxed--categories-setting->struct (cat
					    area
					    path-var
					    pred-name
					    dir-var
					    install-name
					    finalize-install-name
					    remove-name
					    finalize-remove-name)
  (let ((pred (unboxed--function-or-nil pred-name))
	(dir (unboxed--string-or-nil dir-var))
	(install (unboxed--function-or-nil install-name))
	(finalize-install (unboxed--function-or-nil finalize-install-name))
	(remove (unboxed--function-or-nil remove-name))
	(finalize-remove (unboxed--function-or-nil finalize-remove-name)))
    (unless pred ; (and pred (or dir (not install)))
      (signal 'unboxed-invalid-category-spec
	      `(,cat
		,area
		,path-var
		(,pred-name ,pred)
		(,dir-var ,dir)
		(,install-name ,install)
		(,finalize-install-name ,finalize-install)
		(,remove-name ,remove)
		(,finalize-remove-name ,finalize-remove))))
    (unboxed-file-category-create
     :name cat
     :area area
     :path-variable path-var
     :predicate pred
     :location dir
     :install-files install
     :finalize-install-files finalize-install
     :remove-files remove
     :finalize-remove-files finalize-remove)))


(provide 'unboxed-categories)

;;; unboxed-categories.el ends here

;; 
