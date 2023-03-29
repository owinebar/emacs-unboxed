;;; unboxed-database.el        -*- lexical-binding: t; -*-

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

;; Functions for managing areas and the associated unboxed databases

;;; Code:

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

(provide 'unboxed-database)

;;; unboxed-database.el ends here

;; 
