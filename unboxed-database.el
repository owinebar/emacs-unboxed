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

(defun unboxed--area-settings->struct (name boxes db-path cat-settings)
  (let ((cats
	 (mapcar (lambda (args)
		   (let ((cat (apply #'unboxed--categories-setting->struct
				     args)))
		     `(,(unboxed--file-category-name cat) . ,cat)))
		 cat-settings)))
    (unboxed--make-area name boxes db-path cats)))

(defun unboxeds--areas-from-settings (areas-settings)
  (let ((areas
	 (mapcar
	  (lambda (area-settings)
	    (let ((area (apply #'unboxed--area-settings->struct
			       area-settings)))
	      `(,(unboxed--area-name area) . ,area))))))
    areas))

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


(defun unboxed--package-single-p (pd)
  (let ((d (package-desc-dir pd))
	(name (symbol-name (package-desc-name pd)))
	all main auto pkg r)
    (when (and d (file-accessible-directory-p d))
      (setq all (directory-files d nil "^[^.].*$")
	    main (directory-files d nil (concat "^[^.]" (regexp-quote name) "\\.elc?$"))
	    auto (directory-files d nil "^[^.].*-autoloads?\\.elc?$")
	    pkg (directory-files d nil "^[^.].*-pkg\\.elc?$"))
      (when (= (length all) (+ (length main) (length auto) (length pkg)))
	(setq r t)))
    r))


(defun unboxed--package-simple-p (pd)
  (let ((d (package-desc-dir pd))
	(name (symbol-name (package-desc-name pd)))
	(no-subdirs t)
	all fn)
    (setq all (directory-files d t "^[^.].*$"))
    (while (and no-subdirs all)
      (setq fn (pop all)
	    no-subdirs (file-directory-p fn)))
    no-subdirs))

(defun unboxed--package-any-p (pd)
  t)

(defun unboxed--excluded-package-regex (ls)
  (let (re-ls syms re e)
    (while ls
      (setq e (pop ls))
      (cond
       ((symbolp e)
	(push (symbol-name e) syms))
       (t (push e re-ls))))
    (when syms
      (setq e (concat "\\(" (regexp-opt syms) "\\)")))
    (unless (and syms (null re-ls))
      (setq re (pop re-ls))
      (setq e (concat "\\(" re "\\)")))
    (while re-ls
      (setq re (pop re-ls))
      (setq e (concat "\\(" re "\\)\\|" e)))
    e))

(defun unboxed--apply-package-pred (pred excluded-re pd)
  (let (rv)
    (unless (string-match-p excluded-re (unboxed--package-desc-name pd))
      (setq rv (funcall pred pd)))
    rv))

(defun unboxed--packages-to-unbox (db)
  (let ((area (unboxed--sexpr-db-area db))
	(pkgs (unboxed--sexpr-db-packages db))
	unboxed-pkgs pred excluded-re)
    (setq pred (unboxed--area-pred area)
	  excluded-re (unboxed--area-excluded-regex area))
    (maphash (lambda (name pd)
	       (when (unboxed--apply-package-pred pred excluded-re pd)
		 (push pd unboxed-pkgs)))
	     pkgs)
    (reverse unboxed-pkgs)))

(defun unboxed--unbox-package (db pd installed-by-cat)
  (let ((area (unboxed--sexpr-db-area db))
	(pkgs (unboxed--sexpr-db-packages db))
	(installed (unboxed--sexpr-db-installed db))
	(pkg-dir (file-name-as-directory (unboxed-package-desc-dir pd)))
	cats ls install-files cat-pred cat-files noncat-files N
	cat-name cat-installed-files-pair
	cat-installed-files all-installed pr)
    (setq cats (unboxed--area-categories area)
	  ls cats
	  N (length pkg-dir))
    (setq files (mapcar (lambda (fn) (substring fn N))
			(directory-files-recursively pkg-dir "")))
    (while ls
      (setq cat (pop ls)
	    install-files (unboxed-file-category-install-files cat)
	    cat-pred (unboxed-file-category-pred cat))
      (mapc (lambda (fn)
	      (if (funcall pred fn)
		  (push fn cat-files)
		(push fn noncat-files)))
	    files)
      (setq files (nreverse noncat-files)
	    noncat-files nil
	    all-installed (cons (install-files db pd cat-files)
				 all-installed)
	    cat-files nil))
    (setq ls all-installed)
    (while all-installed
      (setq ls (pop all-installed))
      (while ls
	;; reuse the cons cells already allocated above
	(setq pr ls
	      ls (cdr ls)
	      inst (car pr)
	      cat (unboxed-installed-file-category inst)
	      cat-installed-files-pair (assq cat installed-by-cat))
	(setcdr pr (cdr cat-installed-files-pair))
	(setcdr cat-installed-files-pair pr)))
    installed-by-cat))

(defun unboxed--unbox-packages-in-db (db)
  (let ((area (unboxed--sexpr-db-area db))
	(pkgs (unboxed--sexpr-db-packages db))
	(installed (unboxed--sexpr-db-installed db))
	cats installed-by-cat pkgs-to-unbox ls pd)
    (setq cats (unboxed--area-categories area)
	  installed-by-cat (mapcar (lambda (c)
				     `(,(unboxed-file-category-name c)))
				   cats))
    (setq pkgs-to-unbox (unboxed--packages-to-unbox db)
	  ls pkgs-to-unbox)
    (while ls
      (setq pd (pop ls)
	    installed-by-cat (unboxed--unbox-package db pd installed-by-cat)))
    (setq ls cats)
    (while ls
      (setq cat (pop ls)
	    finalize-install-files (unboxed-file-category-finalize-install-files cat)
	    cat-installed-files (cdr (assq cat installed-by-cat))
	    db (funcall finalize-install-files db cat-installed-files)))
    db))

(defun unboxed--unbox-package-list-in-db (db pkg-ls)
  (let ((area (unboxed--sexpr-db-area db))
	(pkgs (unboxed--sexpr-db-packages db))
	(installed (unboxed--sexpr-db-installed db))
	cats installed-by-cat pkgs-to-unbox ls pd)
    (setq cats (unboxed--area-categories area)
	  installed-by-cat (mapcar (lambda (c)
				     `(,(unboxed-file-category-name c)))
				   cats))
    (setq pkgs-to-unbox (unboxed--packages-to-unbox db)
	  ls pkgs-to-unbox)
    (while ls
      (setq pd (pop ls)
	    installed-by-cat (unboxed--unbox-package db pd installed-by-cat)))
    (setq ls cats)
    (while ls
      (setq cat (pop ls)
	    finalize-install-files (unboxed-file-category-finalize-install-files cat)
	    cat-installed-files (cdr (assq cat installed-by-cat))
	    db (funcall finalize-install-files db cat-installed-files)))
    db))

(defun unboxed--rebox-package-list-in-db (db pkg-ls)
  (let ((area (unboxed--sexpr-db-area db))
	(pkgs (unboxed--sexpr-db-packages db))
	(installed (unboxed--sexpr-db-installed db))
	cats cat pkgs-to-unbox ls pd p files-by-cat)
    (setq cats (unboxed--area-categories area)
	  ls pkg-ls
	  files-by-cat (mapcar (lambda (c)
				 `(,(unboxed-file-category-name c)))
			       cats))
    (while ls
      (setq p (pop ls)
	    pd (gethash pkgs p))
      (setq files-by-cat (unboxed--rebox-package-files (db pd files-by-cat))))
    (setq ls cats)
    (while ls
      (setq cat (pop ls)
	    cat-name (unboxed-file-category-name cat)
	    finalize-remove-files (unboxed-file-category-finalize-remove-files cat)
	    cat-files (cdr (assq cat-name files-by-cat))
	    db (funcall finalize-remove-files db cat cat-files)))
    db))

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

(defun unboxed--make-dbs-from-settings (area-settings)
  "Initialize unboxing areas from the value of a customization variable"
  (let ((areas (unboxed--areas-from-settings area-settings))
	dbs)
    (setq dbs
	  (mapcar (lambda (area)
		    (let ((name (unboxed--area-name area)))
		      `(,name
			.
			,(unboxed--create-sexpr-db name areas))))))
    dbs))

(defun unboxed--load-database (area)
  "Load the database associated with area"
  (let ((db-path (unboxed--area-db-path area))
	db)
    (when (
    ))

(defun unboxed--save-database (area-name areas)
  "Save the database associated with area-name in areas"
  )


(provide 'unboxed-database)

;;; unboxed-database.el ends here

;; 
