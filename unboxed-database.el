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

(require 'autoload)
(require 'async-job-queue)
(require 'unboxed-decls)
(require 'unboxed-categories)
(require 'unboxed-file-management)


(defvar unboxed-temp-directory)
(defvar unboxed--package-job-queue-freq 1.0
  "Polling frequency for job queues created during unbox operations.")

(defvar unboxed--catalog-package-chunks 50)

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

(defun unboxed--update-area-autoloads (area _src-loc _dst-loc files _k)
  "Update autoloads file for AREA.
Arguments:
  AREA - Unboxing area"
  (let ((autoloads-fn (unboxed--area-autoloads-file area))
	(area-name (unboxed--area-name area))
	(area-id (intern (format ":%s" (unboxed--area-name area))))
	src inst
	autoloads-file result comp-file new-installed)
    (setq autoloads-file (expand-file-name (file-name-concat dst-loc autoloads-fn))
	  comp-file (unboxed--byte-compiled-library-name autoloads-file)
	  src (unboxed--Csource-file-create
	       :id (intern (format "%s#%s" area-id (file-name-nondirectory autoloads-file)))
	       :package area-id
	       :file (intern (file-name-nondirectory autoloads-file)))
	  inst (unboxed--Cinstalled-file-create
		:src (unboxed--Csource-file-id src)
		:package area-id
		:id (intern (format "%s#%s" area-id (file-name-nondirectory comp-file)))
		:file (intern (file-name-nondirectory comp-file))))
    (let ((al-buffer (get-file-buffer autoloads-file)))
      (when al-buffer
	(with-current-buffer al-buffer
	  (set-buffer-modified-p nil))
	(kill-buffer al-buffer)))
    (make-directory-autoloads dst-loc autoloads-file)
    (let ((default-directory dst-loc))
      (setq result (unboxed--async-byte-compile-file autoloads-file))
      (when (and (stringp result) (> (length result) 0))
	(message "Compile log for %S\n%s" autoloads-file result)))))
  
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
	;; fixme - this needs to be a hash table
	(pkgs (unboxed--sexpr-db-available db))
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
	cat cats ls install-files cat-name cq)
    (setq cats (unboxed--area-categories area)
	  ls cats)
    (unless pkg-cat-files
      (setq pkg-cat-files (unboxed--catalog-package-desc-files pd)))
    (while ls
      (setq cat (cdr (pop ls))
	    cat-name (unboxed-file-category-name cat)
	    install-files (unboxed-file-category-install-files cat)
	    cq (unboxed--get-cat-queue  pkg-cat-files cat-name))
      (if (and cq install-files)
	  (funcall install-files db pd (queue-all cq) ajq installed-files-k)
	(when installed-files-k
	  (funcall installed-files-k nil))))))

(defvar unboxed--unboxed-library-paths
  (mapcar #'locate-library
	  '("queue"
	    "async"
	    "async-job-queue"
	    "rewriting-pcase"
	    "unboxed-decls"
	    "unboxed-custom"
	    "unboxed-categories"
	    "unboxed-file-management"
	    "unboxed-database"))
  "List of paths to load in async jobs calling unboxed functions.")

(defvar unboxed--unboxed-library-paths-loads
  (mapcar (lambda (x) `(load ,x)) unboxed--unboxed-library-paths)
  "Load statements to run at the start of async jobs calling unboxed")

(defvar unboxed--async-pkg-catalog-time-out 600
  "Maximum time allowed for cataloging package files in seconds")

(defun unboxed--async-catalog-packages (db pds id ajq k)
  "Catalog package files in an async job.
Arguments:
  `DB' Database
  `PDS' List of package descriptors to catalog
  `AJQ' job queue
  `K' Continuation to invoke with the cataloged files"
  (let ((pkgs
	 (mapcar (lambda (pd)
		   `(,(unboxed-package-desc-id pd)
		     .
		     ,(unboxed-package-desc-dir pd)))
		 pds))
	(cats (unboxed--sexpr-db-categories db))
	cat-preds job-id finish-k logfile-base
	logfile warnfile msgfile program)
    (setq logfile-base id
	  logfile (unboxed--make-install-logfile "compile-log" nil logfile-base)
	  warnfile (unboxed--make-install-logfile "warnings" nil logfile-base)
	  msgfile (unboxed--make-install-logfile "messages" nil logfile-base)
	  job-id (intern (concat "catalog-files-" id))
	  cat-preds (mapcar (lambda (pr)
			      (cons (car pr)
				    (unboxed-file-category-predicate (cdr pr))))
			    cats)
	  finish-k
	  (lambda (result)
	    (let (log-text warn-text msg-text)
	      ;;(message "%S returned \n%s" job-id (pp result))
	      (message "%S returned" job-id)
	      (setq log-text (unboxed--check-logfile logfile))
	      (setq warn-text (unboxed--check-logfile warnfile))
	      (setq msg-text (unboxed--check-logfile msgfile))
	      (when (> (length log-text) 0)
		(message "Log-text\n===\n%s\n===\n" log-text))
	      (when (> (length warn-text) 0)
		(message "Warn-text\n===\n%s\n===\n" warn-text))
	      (when (> (length msg-text) 0)
		(message "Msg-text\n===\n%s\n===\n" msg-text))
	      (unboxed--add-source-files-to-packages pds result cats)
	      (when k
		(funcall k pds))))
	  program
	  (unboxed--async-expr
	   pd1
	   `(progn 
	      ,@unboxed--unboxed-library-paths-loads
	      (setq pd1 (unboxed--catalog-packages ',pkgs ',cat-preds)))
	   logfile
	   warnfile
	   msgfile))
    ;; (message "Async program\n%s" (pp program))
    (unboxed--simple-schedule ajq program job-id
			      unboxed--async-pkg-catalog-time-out
			      finish-k)))

(defun unboxed--async-unbox-package (txn pd ajq k)
  "Unbox package PD according to db transaction TXN.
Arguments:
  `TXN'  Database transaction
  `PD' Specific package desc to unbox
  `AJQ' job queue
  `K' Continuation to invoke with the list of installed files"
  (let ((db (unboxed--transaction-db txn))
	cats
	cat-preds job-id finish-k logfile-base
	logfile warnfile msgfile program)
    (setq cats (unboxed--sexpr-db-categories db)
	  logfile-base id
	  logfile (unboxed--make-install-logfile "compile-log" nil logfile-base)
	  warnfile (unboxed--make-install-logfile "warnings" nil logfile-base)
	  msgfile (unboxed--make-install-logfile "messages" nil logfile-base)
	  job-id (intern (concat "catalog-files-" id))
	  cat-preds (mapcar (lambda (pr)
			      (cons (car pr)
				    (unboxed-file-category-predicate (cdr pr))))
			    cats)
	  finish-k
	  (lambda (result)
	    (let (log-text warn-text msg-text)
	      ;;(message "%S returned \n%s" job-id (pp result))
	      (message "%S returned" job-id)
	      (setq log-text (unboxed--check-logfile logfile))
	      (setq warn-text (unboxed--check-logfile warnfile))
	      (setq msg-text (unboxed--check-logfile msgfile))
	      (when (> (length log-text) 0)
		(message "Log-text\n===\n%s\n===\n" log-text))
	      (when (> (length warn-text) 0)
		(message "Warn-text\n===\n%s\n===\n" warn-text))
	      (when (> (length msg-text) 0)
		(message "Msg-text\n===\n%s\n===\n" msg-text))
	      (unboxed--add-source-files-to-packages pds result cats)
	      (when k
		(funcall k pds))))
	  program
	  (unboxed--async-expr
	   pd1
	   `(progn 
	      ,@unboxed--unboxed-library-paths-loads
	      (setq pd1 (unboxed--catalog-packages ',pkgs ',cat-preds)))
	   logfile
	   warnfile
	   msgfile))
    ;; (message "Async program\n%s" (pp program))
    (unboxed--simple-schedule ajq program job-id
			      unboxed--async-pkg-catalog-time-out
			      finish-k)))
(defun unboxed--take (n ls)
  (let ((q (make-queue)))
    (while (and ls (> n 0))
      (queue-enqueue q (pop ls))
      (cl-decf n))
    (cons (queue-all q) ls)))
	
(defun unboxed--catalog-packages-in-list (db &optional pkg-ls)
  "Catalog active packages in DB of names in PKG-LS."
  ;;(message "cataloging %S" pkg-ls)
  (unless unboxed-temp-directory
    (setq unboxed-temp-directory (file-name-concat user-emacs-directory
						   "tmp")))
  (setq unboxed-temp-directory
	(file-name-as-directory unboxed-temp-directory))
  (unless (file-accessible-directory-p unboxed-temp-directory)
    (make-directory unboxed-temp-directory t))
  (let ((pds (unboxed--get-db-active-package-descriptors db pkg-ls))
	(idx 0)
	ajq cataloged-pkg-k final-k ls txn head)
    ;;(message "Cataloging \n%S" (queue-map #'unboxed--summarize-package-desc pds))
    (setq txn (unboxed--make-transaction db nil (queue-all pds))
	  cataloged-pkg-k (lambda (pds)
			    (while pds
			      (unboxed--apply-transaction-install-package
			       txn
			       (pop pds))))
	  final-k (lambda (_ajq)
		    (message "Finished cataloging"))
	  ls (queue-all pds)
	  ajq (unboxed--simple-ajq unboxed--package-job-queue-freq
				   final-k
				   'unbox--package-catalog-jobs))
    (while ls
      (setq head (unboxed--take unboxed--catalog-package-chunks ls)
	    ls (cdr head)
	    head (car head)
	    idx (1+ idx))
      (unboxed--async-catalog-packages db
				       head
				       (format "%s" idx)
				       ajq
				       cataloged-pkg-k))))


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


(defun unboxed--get-db-active-package-descriptors (db &optional pkg-ls)
  "Get active package descriptors of DB for names in PKG-LS."
  (let ((pkg-tbl (unboxed--db-packages-named
		  (unboxed--db-state-packages
		   (unboxed--sexpr-db-active db))))
	(pkgs (make-queue)))
    (if pkg-ls
	(maphash (lambda (key pdq)
		   (when (memq key pkg-ls)
		     (queue-enqueue pkgs (queue-first pdq))))
		 pkg-tbl)
      (maphash (lambda (_key pdq)
		 (queue-enqueue pkgs (queue-first pdq)))
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
	(pkgs-to-unbox
	 (queue-all (unboxed--get-db-active-package-descriptors db pkg-ls)))
	ajq finalize-pkg-files-k
	cats ls
	installed-file-k txn)
    (setq cats (unboxed--area-categories area))
    (unboxed--ensure-category-locations cats)
    (setq txn (unboxed--make-transaction db nil pkgs-to-unbox)
	  installed-file-k (lambda (inst)
			     (unboxed--apply-transaction-install-file
			      txn
			      inst))
	  finalize-pkg-files-k (lambda (ajq)
				 (unboxed--finalize-unbox-package-list
				  ajq db cats txn))
	  ajq (unboxed--simple-ajq unboxed--package-job-queue-freq
				   finalize-pkg-files-k
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
	db area box-paths boxed-pkgs active-pkgs test-pd
	library-loc cats lib-cat al-fn)
    ;; Only record areas in scope
    ;; E.G. site package database should not contain references to user database file
    ;;     of site administrators
    (setq active-pkgs (unboxed--db-state-packages active))
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
    (message "activated initial\n%S" (unboxed--summarize-db-packages active-pkgs))
    (let ((ls package-activated-list)
	  p pd)
      (while ls
	(setq p (pop ls))
	(when (assq p boxed-pkgs)
	  (setq test-pd (unboxed--get-package-from-db-packages-by-name active-pkgs p))
	  (when test-pd
	    (message "Package %S is already in active packages" p))
	  (setq pd (unboxed--get-package-from-db-packages-by-name available p))
	  (unless pd
	    (message "Package %S is nil" p))
	  (message "Package %S\n%S" p (unboxed--summarize-package-desc pd))
	  (when pd
	    (message "Package %S is being added" p)
	    (unboxed--add-package-to-db-state active pd)
	    ;; validate
	    (setq test-pd (unboxed--get-package-from-db-packages-by-name active-pkgs p))
	    (unless (eq test-pd pd)
	      (message "Package %s did not get added - %S versus %S"
		       p
		       (unboxed--summarize-package-desc test-pd)
		       (unboxed--summarize-package-desc pd)))))))
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
