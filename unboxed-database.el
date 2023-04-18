;;; unboxed-database.el --- database operations for unboxed
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

(require 'unboxed-decls)
(require 'unboxed-categories)
(require 'unboxed-file-management)

(defun unboxed--installed-file-key (inst)
  "Key for installed-file INST in installed field of database."
  (file-name-concat
   (symbol-name (unboxed-installed-file-category inst))
   (unboxed-installed-file-file inst)))

(defun unboxed--package-in-boxes (pd boxes)
  "Test whether package PD is on the paths in BOXES."
  ;(setq boxes (mapcar #'expand-file-name boxes))
  (let ((d (package-desc-dir pd))
	result)
    (when (and d (stringp d))
      (setq d (expand-file-name d)
	    result (seq-filter (lambda (p) (string-prefix-p p d)) boxes)))
    result))

(defun unboxed--init-package-desc (mgr pd version)
  "Initialize unboxed package descriptor from package-desc PD.
Arguments:
  MGR - package installation manager, `package' or `unboxed'
  PD - unboxed package description
  VERSION - version string from boxed directory of PD"
  (let ((s (unboxed-package-desc-create :manager 'package
					:version-string version))
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
			 pd)))
    s))

(defun unboxed--area-settings->struct (name
				       boxes db-path
				       pred excluded
				       theme-libs datadir-pats
				       patches autoloads
				       cat-settings)
  "Create area struct from customization value.
Arguments:
  NAME -
  BOXES - paths of boxed packages in the area
  DB-PATH - path to db
  PRED - Test
  EXCLUDED - exclude
  THEME-LIBS
  DATADIR-PATS -
  PATCHES - alist of packages and patch files to use in place of rewriting
  AUTOLOADS - base name of autoloads file to generate in the libraries directory
  CAT-SETTINGS - category parameters in customization format"
  (let ((cats
	 (mapcar (lambda (args)
		   (let ((cat (apply #'unboxed--categories-setting->struct
				     args)))
		     `(,(unboxed-file-category-name cat) . ,cat)))
		 cat-settings)))
    (unboxed--make-area name
			boxes db-path
			pred excluded
			theme-libs datadir-pats
			patches autoloads
			cats)))

(defun unboxed--areas-from-settings (areas-settings)
  "Create alist of area structures from AREAS-SETTINGS."
  (let ((areas
	 (mapcar
	  (lambda (area-settings)
	    (let ((area (apply #'unboxed--area-settings->struct
			       area-settings)))
	      `(,(unboxed--area-name area) . ,area)))
	  areas-settings)))
    areas))

(defun unboxed--scoped-areas (area-name areas)
  "Return tail of AREAS alist with first association key eq to AREA-NAME."
  (let (pr)
    (while (and (null pr) areas)
      (setq pr (pop areas))
      (unless (eq (car pr) area-name)
	(setq pr nil)))
    (when pr
      (push pr areas)))
  areas)

(defun unboxed--scoped-libdirs (db)
  "List library directories from areas in dependency scope of DB."
  (let ((scoped-areas
	 (unboxed--scoped-areas
	  (unboxed--area-name (unboxed--sexpr-db-area db))
	  (unboxed--sexpr-db-areas db)))
	libdirs)
    (setq libdirs
	  (mapcar (lambda (a)
		    (unboxed--area-category-location a 'library))
		  scoped-areas))
    libdirs))

(defun unboxed--scoped-autoloads (db)
  "List autoload files from areas in dependency scope of DB."
  (let ((scoped-areas
	 (unboxed--scoped-areas
	  (unboxed--area-name (unboxed--sexpr-db-area db))
	  (unboxed--sexpr-db-areas db)))
	als)
    (setq als
	  (mapcar (lambda (a)
		    (unboxed--area-autoloads-file a))
		  scoped-areas))
    (nreverse als)))

(defun unboxed--excluded-package-regex (ls)
  "Construct regular expression to match the package names on LS."
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
  "Apply PRED to PD if its name does not match EXCLUDED-RE."
  (let (rv)
    (unless (string-match-p excluded-re
			    (symbol-name (unboxed-package-desc-name pd)))
      (setq rv (funcall pred pd)))
    rv))

(defun unboxed--packages-to-unbox (db)
  "List packages from boxed area of DB matching its predicate."
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
  "Unbox package PD in DB with INSTALLED-BY-CAT alist of installed-files."
  (message "Unboxing %s" pd)
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
      (setq cat (cdr (pop ls))
	    install-files (unboxed-file-category-install-files cat)
	    cat-pred (unboxed-file-category-predicate cat))
      (mapc (lambda (fn)
	      (if (funcall cat-pred fn)
		  (push fn cat-files)
		(push fn noncat-files)))
	    files)
      (setq files (nreverse noncat-files)
	    noncat-files nil)
      (when install-files
	(setq all-installed (cons (funcall install-files db pd cat-files)
				  all-installed)))
      (setq cat-files nil))
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
  "Unbox all packages in DB that satisfy its predicate."
  (unboxed--unbox-packages-in-list
   db
   (unboxed--packages-to-unbox db)))

(defun unboxed--unbox-package-list-in-db (db pkg-ls)
  "Unbox packages in PKG-LS in DB."
  (unless unboxed-temp-directory
    (setq unboxed-temp-directory (file-name-concat user-emacs-directory
						   "tmp")))
  (setq unboxed-temp-directory
	(file-name-as-directory unboxed-temp-directory))
  (unless (file-accessible-directory-p unboxed-temp-directory)
    (make-directory unboxed-temp-directory t))
  (let ((area (unboxed--sexpr-db-area db))
	(pkgs (unboxed--sexpr-db-packages db))
	(installed (unboxed--sexpr-db-installed db))
	cats installed-by-cat pkgs-to-unbox ls pd
	cat-name new-installed cat loc)
    (maphash (lambda (key value)
	       (when (memq key pkg-ls)
		 (push value pkgs-to-unbox)))
	     pkgs)
    (setq cats (unboxed--area-categories area)
	  ls cats
	  installed-by-cat (mapcar
			    (lambda (c-pr)
			      `(,(unboxed-file-category-name (cdr c-pr))))
			    cats))
    (while ls
      (setq cat (cdr (pop ls)))
      (setq loc (unboxed-file-category-location cat))
      (when (stringp loc)
	(unless (file-accessible-directory-p loc)
	  (make-directory loc t))))
    (setq ls pkgs-to-unbox)
    (while ls
      (setq pd (pop ls)
	    installed-by-cat (unboxed--unbox-package db pd installed-by-cat)))
    (setq ls cats)
    (while ls
      (setq cat (cdr (pop ls))
	    cat-name (unboxed-file-category-name cat)
	    finalize-install-files (unboxed-file-category-finalize-install-files cat)
	    cat-installed-files (cdr (assq cat-name installed-by-cat)))
      (mapc (lambda (inst)
	      (puthash (unboxed--installed-file-key inst)
		       inst
		       installed))
	    cat-installed-files)
      (when finalize-install-files
	(setq new-installed (funcall finalize-install-files db
					    cat cat-installed-files))
	(mapc (lambda (inst)
		(puthash (unboxed--installed-file-key inst)
			 inst
			 installed))
	      new-installed)))
    (mapc (lambda (pd)
	    (setf (unboxed-package-desc-manager pd) 'unboxed))
	  pkgs-to-unbox)
    db))

(defun unboxed--rebox-package-list-in-db (db pkg-ls)
  "Rebox packages in PKG-LS of DB."
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

(defun unboxed--ensure-autoloads-file (al-fn)
  "Create the autoloads file AL-FN if it does not exist already."
  (when (file-writable-p al-fn)
    (let ((feature (intern
		    (file-name-sans-extension
		     (file-name-nondirectory al-fn))))
	  text)
      (unless (file-exists-p al-fn)
	(require 'autoload)
	(setq text (autoload-rubric al-fn nil feature)
	      text (replace-regexp-in-string
		    "^;+[[:space:]]*no-byte-compile:[[:space:]]+t[[:space:]]*\n"
		    ""
		    text))
	(with-temp-buffer
	  (insert text)
	  (write-region nil nil al-fn))))))

(defun unboxed--create-sexpr-db (area-name areas)
  "Create an unboxed db for AREA-NAME defined in alist AREAS."
  (let (area box-paths boxed-pkgs pkgs inst autoloads library-loc cats lib-cat al-fn)
    ;; Only record areas in scope
    ;; E.G. site package database should not contain references to user database file
    ;;     of site administrators
    (setq areas (unboxed--scoped-areas area-name areas))
    (setq area (cdar areas))
    (setq cats (unboxed--area-categories area))
    (setq lib-cat (cdr (assq 'library cats)))
    (setq library-loc (unboxed-file-category-location lib-cat))
    (setq al-fn (file-name-concat library-loc
				  (unboxed--area-autoloads-file area)))
    (unboxed--ensure-autoloads-file (expand-file-name al-fn))
    (setq box-paths (mapcar #'expand-file-name (unboxed--area-boxes area)))
    (setq pkgs (make-hash-table :test #'eq))
    (message "Box Paths: %S" box-paths)
    (setq boxed-pkgs
	  (seq-filter
	   (lambda (pr)
	     (let ((pd (cadr pr))
		   paths)
	       (setq paths (unboxed--package-in-boxes pd box-paths))
	       (and paths pr)))
	   package-alist))
    (message "area packages %S" (length boxed-pkgs))
    (mapc (lambda (pd-pr)
	    (let ((pd (cadr pd-pr))
		  (version "")
		  pkg pkg-dir pkg-prefix
		  upd)
	      (setq pkg-dir (package-desc-dir pd))
	      (setq pkg (package-desc-name pd))
	      (setq pkg-prefix (concat (symbol-name pkg) "-"))
	      (when (stringp pkg-dir)
		(when (directory-name-p pkg-dir)
		  (setq pkg-dir (directory-file-name pkg-dir)))
		(setq pkg-dir (file-name-nondirectory pkg-dir))
		(when (string-prefix-p pkg-prefix pkg-dir)
		  (setq version (substring pkg-dir (length pkg-prefix)))))
	      (setq upd (unboxed--init-package-desc 'package pd version))
	      (setf (unboxed-package-desc-single upd)
		    (unboxed-package-single-p pd))
	      (setf (unboxed-package-desc-simple upd)
		    (unboxed-package-simple-p pd))
	      (puthash (unboxed-package-desc-name upd) upd pkgs)))
	  boxed-pkgs)
    ;; these record installed files managed by unboxed, so they start out empty
    (setq inst (make-hash-table :test #'equal))
    (unboxed--sexpr-db-create
     :layouts unboxed--struct-layouts
     :areas areas
     :area area
     :packages pkgs
     :installed inst)))

(defun unboxed--make-dbs-from-settings (area-settings)
  "Initialize unboxing areas from the value of a customization variable AREA-SETTINGS."
  (let ((areas (unboxed--areas-from-settings area-settings))
	dbs)
    (setq dbs
	  (mapcar (lambda (area-pair)
		    (let ((name (unboxed--area-name (cdr area-pair))))
		      `(,name
			.
			,(unboxed--create-sexpr-db name areas))))
		  areas))
    dbs))

;;; FIXME: should validate db structure
(defun unboxed--load-database (area)
  "Load the database associated with AREA."
  (let ((db-path (unboxed--area-db-path area))
	db)
    (when (file-readable-p db-path)
      (with-temp-buffer
	(insert-file-contents db-path)
	(setq db (read (current-buffer)))))
    db))

(defun unboxed--save-database (db)
  "Save the database DB."
  (let ((db-path (unboxed--area-db-path area)))
    (with-temp-buffer
      (pp db (current-buffer))
      (if (file-exists-p db-path)
	  (delete-file db-path))
      (write-region (point-min) (point-max) db-path 'excl))
    nil))



(provide 'unboxed-database)

;;; unboxed-database.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ajq-" . "async-job-queue-")("ub-" . "unboxed-"))
;; End:
;; 
