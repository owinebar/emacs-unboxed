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

(defun unboxed--package-in-boxes (pd boxes)
  (let ((d (package-desc-dir pd)))
    (and d
	 (stringp d)
	 (seq-filter (lambda (p) (string-prefix-p p d)) boxes))))


(defun unboxed--init-package-desc (mgr pd)
  "Initialize an unboxed package descriptor from the manager field and a pre-existing package-desc"
  (let ((s (unboxed-package-desc-create :manager 'package))
	n)
    (setq n (length pd))
    (if (recordp s)
	(let ((i 1))
	  (while (< i n)
	    (aset s i (aref pd i))
	    (cl-incf i)))
      (let ((i0 1))
	(unless (eq (seq-elt s 0) 'unboxed-package-desc)
	  (setq i0 0))
	(seq-map-indexed (lambda (elt idx)
			   (when (>= idx i0)
			     (setf (seq-elt s idx) elt)))
			 pd)))))

(defun unboxed--make-area-from-setting (name boxes db-path cats)
  (unboxed--make-area
   name boxes db-path
   ,(mapcar (lambda (args)
	      (let ((cat (apply #'unboxed--categories-setting->struct args)))
		`(,(unboxed--file-category-name cat) . ,cat)))
	    cats)))

(defun unboxed--scoped-areas (area-name areas)
  "Return tail of areas alist with first association key eq to area-name"
  (let (pr)
    (while (and (null pr) areas)
      (setq pr (pop areas))
      (unless (eq (car pr) area-name)
	(setq pr nil)))
    (when pr
      (push pr areas)))
  areas)

    
(defun unboxed--create-sexpr-db (area-name areas)
  "Create an unboxed db in the sexpr format - initialize from package-desc
table assuming no packages have been unboxed"
  (let (area box-paths pkgs inst)
    ;; Only record areas in scope
    ;; E.G. site package database should not contain references to user database file
    ;;     of site administrators
    (setq areas (unboxed--scoped-areas area-name areas))
    (setq area (cdar areas))
    (setq box-paths (unboxed--area-boxes area))
    (setq pkgs (make-hash-table :test #'eq))
    (mapc (lambda (pd)
	    (let ((upd (unboxed--init-package-desc 'package pd)))
	      (puthash pkgs (unboxed-package-desc-name upd) upd)))
	  (seq-filter
	   (lambda (pr)
	     (let ((pd (cdr pr))
		   paths)
	       (setq paths (unboxed--package-in-boxes pd box-paths))
	       (and paths pr)))
	   (package-alist)))
    ;; these record installed files managed by unboxed, so they start out empty
    (setq inst (make-hash-table :test #'equal))
    (unboxed--sexpr-db-create
     :layouts unboxed--struct-layouts
     :areas areas
     :packages pkgs
     :installed inst)))

(defun unboxed--make-db (area-name areas)
  (let ((area (cdr (assq area-name areas)))
	categories area)
    (setq categories (unboxed--area-categories area))
    (setq db (unboxed--create-sexpr-db area-name proto-areas categories))
    (setq area
	  (unboxed--area-config-create
	   :name (unboxed--proto-area-name proto-area)
	   :boxes (unboxed--proto-area-boxes proto-area)
	   :db-path (unboxed--proto-area-db-path proto-area)
	   :db db))
    area))

(defun unboxed--make-dbs-from-settings (area-settings)
  "Initialize unboxing areas from the value of a customization variable"
  (let ((proto-areas
	 (mapcar (lambda (area-setting)
		   (let ((pa (apply #'unboxed--make-proto-area-from-settings area-setting)))
		     `(,(unboxed--proto-area-name pa) . ,pa)))
		 area-settings))
	arg-pairs areas)
    (setq areas
	  (cl-mapcar (lambda (pa)
		       (unboxed--make-area
			(unboxed--proto-area-name pa)
			proto-areas))
		     proto-areas))
    areas))

(defun unboxed--load-database (area-name areas)
  "Load the database associated with area-name in areas"
  (let ((area (cdr (assq area-name areas)))
	db-path db)
    
  )
(defun unboxed--save-database (area-name areas)
  "Save the database associated with area-name in areas"
  )


(provide 'unboxed-database)

;;; unboxed-database.el ends here

;; 
