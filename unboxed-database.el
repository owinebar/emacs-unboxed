;;; unboxed-database.el --- database operations for unboxed    -*- lexical-binding: t; -*-

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

(require 'async-job-queue)
(require 'unboxed-decls)
(require 'unboxed-categories)
(require 'unboxed-file-management)

(defun unboxed--package-in-boxes (pd boxes)
  "Test whether package PD is on the paths in BOXES."
  ;(setq boxes (mapcar #'expand-file-name boxes))
  (let ((d (package-desc-dir pd))
	result)
    (when (and d (stringp d))
      (setq d (expand-file-name d)
	    result (seq-filter (lambda (p) (string-prefix-p p d)) boxes)))
    result))

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
    (maphash (lambda (_name pd)
	       (when (unboxed--apply-package-pred pred excluded-re pd)
		 (push pd unboxed-pkgs)))
	     pkgs)
    (reverse unboxed-pkgs)))

(defun unboxed--unbox-package (db pd &optional installed-files-k ajq)
  "Unbox package PD in DB with INSTALLED-BY-CAT alist of installed-files."
  (message "Unboxing %s" pd)
  (let ((area (unboxed--sexpr-db-area db))
	(pkg-cat-files (unboxed--db-files-locations (unboxed-package-desc-files pd)))
	cats ls install-files cat-name cq)
    (setq cats (unboxed--area-categories area)
	  ls cats)
    (while ls
      (setq cat (cdr (pop ls))
	    cat-name (unboxed-file-category-name cat)
	    install-files (unboxed-file-category-install-files cat)
	    cq (unboxed--get-cat-queue  pkg-cat-files cat-name))
      (if (and cq install-files)
	  (funcall install-files db pd (queue-all cq) ajq installed-files-k)
	(when installed-files-k
	  (funcall installed-files-k nil))))))

(defun unboxed--unbox-packages-in-db (db)
  "Unbox all packages in DB that satisfy its predicate."
  (unboxed--unbox-packages-in-list
   db
   (unboxed--packages-to-unbox db)))

(defun unboxed--ensure-category-locations (cats)
  "Ensure locations of file categories in CATS alist exist."
  (let ((ls cats)
	cat loc)
    (while ls
      (setq cat (cdr (pop ls)))
      (setq loc (unboxed-file-category-location cat))
      (when (stringp loc)
	(unless (file-accessible-directory-p loc)
	  (make-directory loc t))))))


(defvar unboxed--package-job-queue-freq 1.0
  "Polling frequency for job queues created during unbox operations.")

(defun unboxed--get-db-active-package-descriptors (db &optional pkg-ls)
  "Get active package descriptors of DB for names in PKG-LS."
  (let ((pkg-tbl (unboxed--db-packages-descs
		  (unboxed--sexpr-db-active db)))
	(pkgs (make-queue)))
    (if pkg-ls
	(maphash (lambda (key pd)
		   (when (memq key pkg-ls)
		     (queue-enqueue pkgs pd)))
		 pkg-tbl)
      (maphash (lambda (_key pd)
		 (queue-enqueue pkgs pd))
	       pkg-tbl))
    pkgs))


(defun unboxed--unbox-packages-in-list (db pkg-ls)
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
	ajq finalize-pkg-files-k
	cats pkgs-to-unbox ls
	installed-file-k txn)
    (maphash (lambda (key value)
	       (when (memq key pkg-ls)
		 (push value pkgs-to-unbox)))
	     pkgs)
    (setq cats (unboxed--area-categories area))
    (unboxed--ensure-category-locations cats)
    (setq txn (unboxed--make-transaction db nil pkgs-to-unbox)
	  installed-file-k (lambda (inst)
			     (unboxed--enact-transaction-install-file txn inst))
	  finalize-pkg-files-k (lambda (ajq)
				 (unboxed--finalize-unbox-package-list
				  ajq db cats txn))
	  ajq (ajq-make-job-queue unboxed--package-job-queue-freq
				  nil
				  finalize-pkg-files-k
				  nil
				  nil
				  nil
				  'unbox-install-jobs)
	  ls pkgs-to-unbox)
    (while ls
      (unboxed--unbox-package db (pop ls) installed-file-k ajq))))

(defun unboxed--finalize-unbox-package-list (_ajq db cats txn)
  "Finalize unboxing transaction TXN in database DB.
Arguments:
  _AJQ - Job queue used for installing package-specific files
  DB - package database
  CATS - file categories from DB
  TXN - transaction record"
  (let ((ls cats)
	(installed
	 (unboxed--db-state-files
	  (unboxed--db-delta-install
	   (unboxed--transaction-done txn))))
	(pkgs-unboxed
	 (unboxed--db-packages-descs
	  (unboxed--db-state-packages
	   (unboxed--db-delta-install
	    (unboxed--transaction-done txn)))))
	ajq
	installed-by-cat q
	cat cat-name finalize-install-files
	finalize-installed-files-k final-k)
    (setq installed-by-cat (unboxed--db-files-locations installed))
    (while ls
      (setq cat (cdr (pop ls))
	    cat-name (unboxed-file-category-name cat)
	    finalize-install-files (unboxed-file-category-finalize-install-files cat)
	    q (unboxed--get-cat-queue installed-by-cat cat-name)
	    final-k (lambda (_ajq)
		      (mapc (lambda (_key pd)
			      (setf (unboxed-package-desc-manager pd) 'unboxed))
			    pkgs-unboxed))
	    finalize-installed-files-k (lambda (inst)
					 (unboxed--add-installed-file inst installed))
	    ajq (ajq-make-job-queue unboxed--package-job-queue-freq
				    nil
				    final-k
				    nil
				    nil
				    nil
				    'unbox-finalize-install-jobs))
      (when finalize-install-files
	(funcall finalize-install-files
		 db
		 cat
		 q
		 txn
		 ajq
		 finalize-installed-files-k)))
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
  (let ((available (unboxed--db-packages-create))
	(active (unboxed--db-state-create))
	db area box-paths boxed-pkgs
	library-loc cats lib-cat al-fn)
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
    (message "Box Paths: %S" box-paths)
    (setq db (unboxed--sexpr-db-create
	      :layouts unboxed--struct-layouts
	      :areas areas
	      :area area
	      :available available
	      :active active))
    (setq boxed-pkgs
	  (seq-filter
	   (lambda (pr)
	     (let ((name (car pr))
		   (pds (cdr pr)))
	       (setq pds (seq-filter
			  (lambda (pd)
			    (unboxed--package-in-boxes pd box-paths))
			  pds))
	       (and pds `(,name ,@pds))))
	   package-alist))
    (message "area packages %S" (length boxed-pkgs))
    (mapc (lambda (pr)
	    (mapc (lambda (pd)
		    (unboxed--add-package-to-db-packages
		     available
		     (unboxed--make-package-desc db pd)))
		  (cdr pr)))
	  boxed-pkgs)
    (let ((ls package-activated-list)
	  p pd)
      (while ls
	(setq p (pop ls))
	(setq pd (unboxed--get-package-from-state-by-name available p))
	(when pd
	  (unboxed--add-package-to-db-state active pd))))
    db))

(defun unboxed--make-dbs-from-settings (area-settings)
  "Make unboxing dbs from AREA-SETTINGS."
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
  (let ((db-path (unboxed--sexpr-db-path db))
	(print-circle t))
    (with-temp-buffer
      (pp db (current-buffer))
      (if (file-exists-p db-path)
	  (delete-file db-path))
      (write-region (point-min) (point-max) db-path 'excl))
    nil))



(provide 'unboxed-database)

;;; unboxed-database.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ajq-" . "async-job-queue-")("ub-" . "unboxed-")("q-" . "queue-"))
;; End:
;; 
