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

(defun unboxed--make-initial-area (name boxes db-path cats)
  `(,(unboxed--area-config-create
      :name name
      :boxes boxes
      :db-path db-path)
    ,@(mapcar (lambda (args)
		(apply #'unboxed--categories-setting->struct args))
	      cats)))

;; dummy-areas are area-configs with db component set to nil
(defun unboxed--create-sexpr-db (area-name dummy-areas cats)
  "Create an unboxed db in the sexpr format - initialize from package-desc
table assuming no packages have been unboxed"
  (let (dummy-area area-configs box-paths pkgs inst)
    (setq dummy-area (cdr (assq area-name dummy-areas)))
    (setq box-paths (unboxed--area-config-boxes dummy-area))
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
     :areas dummy-areas
     :categories cats
     :packages pkgs
     :installed inst)))

(defun unboxed--make-areas (area-settings)
  (let ((dummy-areas (mapcar #'unboxed--make-initial-area))
	arg-pairs areas)
    (setq arg-pairs (mapcar (lambda (pr)
			      (cons (unboxed--area-config-name (car pr))
				    (cdr pr)))
			    dummy-areas))
    (setq dummy-areas (mapcar #'car dummy-areas))
    (setq areas
	  (cl-mapcar (lambda (pr)
		       (unboxed--create-sexpr-db (car pr)
						 dummy-areas
						 (cdr pr)))
		     arg-pairs))
    areas))
		       

(provide 'unboxed-database)

;;; unboxed-database.el ends here

;; 
