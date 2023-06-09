;;; unboxed-decls.el --- Structure declarations for unboxed        -*- lexical-binding: t; -*-

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

;; Data structures declarations for unboxed package management

;;; Code:

(require 'package)
(require 'queue)
(require 'async-job-queue)

(defvar unboxed--buffer-name "*Unboxed*"
  "Name of unboxed logging buffer.")

(defvar unboxed--buffer (get-buffer-create unboxed--buffer-name)
  "The unboxed logging buffer.")

(defun queue-map (f q)
  "Map function F over queue Q, returning a queue."
  (let ((q1 (make-queue))
	(ls (queue-all q)))
    (while ls
      (queue-enqueue q1 (funcall f (pop ls))))
    q1))

(defun queue-remq (q elt)
  "Remove all occurence of ELT from queue Q."
  (let ((ls (delq elt (queue-all q)))
	tail0 tail1)
    (setq tail0 ls)
    (when (consp tail0)
      (setq tail1 (cdr tail0)))
    (while tail1
      (setq tail1 (cdr (setq tail0 (cdr tail0)))))
    (setf (queue-head q) ls)
    (setf (queue-tail q) tail0))
  q)

(defun queue-remove (q elt)
  "Remove all occurence of ELT from queue Q."
  (let ((head (queue-all q))
	 ls
	tail0 tail1)
    (setq ls (delete elt head)
	  tail0 ls)
    (when (consp tail0)
      (setq tail1 (cdr tail0)))
    (while tail1
      (setq tail1 (cdr (setq tail0 (cdr tail0)))))
    (setf (queue-head q) ls)
    (setf (queue-tail q) tail0))
  q)

(defun queue-filter (q pred)
  "Remove all elements satisfying PRED from queue Q."
  (let ((head (queue-all q))
	 ls
	tail0 tail1)
    (setq ls (seq-filter pred head)
	  tail0 ls)
    (when (consp tail0)
      (setq tail1 (cdr tail0)))
    (while tail1
      (setq tail1 (cdr (setq tail0 (cdr tail0)))))
    (setf (queue-head q) ls)
    (setf (queue-tail q) tail0))
  q)

(defun unboxed--get-package-desc-version (pd)
  "Get the version string from package-desc PD directory name."
  (let ((version "")
	pkg pkg-dir pkg-prefix)
    (setq pkg-dir (package-desc-dir pd))
    (setq pkg (package-desc-name pd))
    (setq pkg-prefix (concat (symbol-name pkg) "-"))
    (when (stringp pkg-dir)
      (when (directory-name-p pkg-dir)
	(setq pkg-dir (directory-file-name pkg-dir)))
      (setq pkg-dir (file-name-nondirectory pkg-dir))
      (when (string-prefix-p pkg-prefix pkg-dir)
	(setq version (substring pkg-dir (length pkg-prefix)))))
    version))

(cl-defstruct (unboxed--area
	       (:constructor unboxed--area-create)
	       (:copier unboxed--area-copy))
  "Structure recording the parameters for an unboxed area, e.g. for user
or site packages
  Slots:
  `name' Name of the package area, e.g. user or site
  `boxes' Directories containing the boxed packages
  `db-path' Path to the db file
  `pred' Predicate to determine whether to a package in this area should
         be unboxed
  `excluded' Packages that are never unboxed in this area
  `excluded-regex' Regular expression derived from excluded
  `theme-libraries' ELisp libraries ending in `-theme' in this area
  `datadir-pats' Data directory pcase patterns for rewriting
  `patches' Package-specific patches in this area
  `autoloads-file' Name of generated autoloads file for unboxed libraries
  `system-load-path' load-path set in \"emacs -Q\" invocation
  `categories' Assoc list of file-categories."
  name
  boxes
  db-path
  pred
  excluded
  excluded-regex
  theme-libraries
  datadir-pats
  patches
  autoloads-file
  system-load-path
  categories)



(defun unboxed--summarize-area (area)
  "Summarize unboxing area AREA."
  (when area
    `(area
      (name ,(unboxed--area-name area))
      (boxes ,(unboxed--area-boxes area))
      (db-path ,(unboxed--area-db-path area))
      (pred ,(and (unboxed--area-pred area) t))
      (excluded ,(unboxed--area-excluded area))
      (excluded-regex ,(unboxed--area-excluded-regex area))
      (theme-libraries ,(unboxed--area-theme-libraries area))
      (datadir-pats ,(unboxed--area-datadir-pats area))
      (patches ,(unboxed--area-patches area))
      (autoloads-file ,(unboxed--area-autoloads-file area))
      (system-load-path ,(unboxed--area-system-load-path area))
      (categories
       ,(mapcar (lambda (pr)
		     `(,(car pr)
		       ,(unboxed--summarize-file-category (cdr pr))))
		   (unboxed--area-categories area))))))


(cl-defstruct (unboxed--db-files
	       (:constructor unboxed--db-files-create)
	       (:copier unboxed--db-files-copy))
  "Collection of files associated with packages in db.
  Slots:
  `files' - set of unboxed source- or installed- file objects
  `locations' - list of source- or installed- files associated to each category"
  (files (make-hash-table))
  (locations (make-queue)))

(defun unboxed--summarize-source-file-cat-queue (cq)
  "Summarize category queue CQ with source-file entries"
  (when cq
    `(source-file-category-queue
      ,(mapcar (lambda (pr)
		 (let ((cat (car pr))
		       (q (cdr pr)))
		   `(,cat (queue ,(mapcar #'unboxed--summarize-source-file (queue-all q))))))
	       (queue-all cq)))))

(defun unboxed--summarize-source-db-files (srcs)
  "Summarize db-files collection of source files SRCS."
  (when srcs
    `(db-files
      (files ,(let ((r (make-queue)))
		(maphash (lambda (id q)
			   (queue-enqueue r `(,id (queue ,(mapcar #'unboxed--summarize-source-file (queue-all q))))))
			 (unboxed--db-files-files srcs))
		(queue-all r)))
      (locations
       ,(unboxed--summarize-source-file-cat-queue (unboxed--db-files-locations srcs))))))
  
(defun unboxed--summarize-installed-db-files (insts)
  "Summarize db-files collection of installed files INSTS."
  (when insts
    `(db-files
      (files ,(let ((r (make-queue)))
		(maphash (lambda (id q)
			   (queue-enqueue r `(,id (queue ,(mapcar #'unboxed--summarize-installed-file (queue-all q))))))
			 (unboxed--db-files-files insts))
		(queue-all r)))
      (locations
       (queue
	,(let ((r (make-queue)))
	   (mapc (lambda (pr)
		   (let ((cat (car pr))
			 (q (cdr pr)))
		     (queue-enqueue r `(,cat (queue ,(mapcar #'unboxed--summarize-installed-file (queue-all q)))))))
		 (unboxed--db-files-locations insts))
	   (queue-all r)))))))

(cl-defstruct (unboxed--db-packages
	       (:constructor unboxed--db-packages-create)
	       (:copier unboxed--db-packages-copy))
  "Collection of package descriptors which all belong to the same db.
  Slots:
  `descs' map of versioned package names to unboxed package desc object
  `named' - map package symbols to queue of versions in boxed area"
  (descs (make-hash-table))
  (named (make-hash-table)))

(defun unboxed--summarize-db-packages (pkgs)
  "Summarize db-packages collection  PKGS."
  (when pkgs
    `(db-packages
      (descs ,(let ((r (make-queue)))
		(maphash (lambda (id q)
			   (queue-enqueue r `(,id ,(queue-length q) (queue ,(mapcar #'unboxed--summarize-package-desc (queue-all q))))))
			 (unboxed--db-packages-descs pkgs))
		(queue-all r)))
      (named ,(let ((r (make-queue)))
		(maphash (lambda (id q)
			   (queue-enqueue r `(,id ,(queue-length q) (queue ,(mapcar #'unboxed--summarize-package-desc (queue-all q))))))
			 (unboxed--db-packages-named pkgs))
		(queue-all r))))))


(cl-defstruct (unboxed--db-state
	       (:constructor unboxed--db-state-create)
	       (:copier unboxed--db-state-copy))
  "Database state
  Slots:
  `packages' - unboxed--db-packages collection
  `files' - unboxed--db-files collection"
  (packages (unboxed--db-packages-create))
  (files (unboxed--db-files-create)))

(defun unboxed--summarize-db-state (state)
  "Summarize db-state  STATE."
  (when state
    `(db-state
      (packages ,(unboxed--summarize-db-packages (unboxed--db-state-packages state)))
      (files ,(unboxed--summarize-installed-db-files (unboxed--db-state-files state))))))


;;; this delta is for changes against activated packages
(cl-defstruct (unboxed--db-delta
	       (:constructor unboxed--db-delta-create)
	       (:copier unboxed--db-delta-copy))
  "The difference between two database states.
  Slots:
  `remove' the db state to eliminate
  `install' the db state requiring installation"
  (remove (unboxed--db-state-create))
  (install (unboxed--db-state-create)))

(defun unboxed--summarize-db-delta (delta)
  "Summarize db-delta  DELTA."
  (when delta
    `(db-delta
      (remove ,(unboxed--summarize-db-state (unboxed--db-delta-remove delta)))
      (install ,(unboxed--summarize-db-state (unboxed--db-delta-install delta))))))


;;; this delta is for changes in available packages
(cl-defstruct (unboxed--db-packages-delta
	       (:constructor unboxed--db-packages-delta-create)
	       (:copier unboxed--db-packages-delta-copy))
  "The difference between two sets of packages.
  Slots:
  `remove' the db-packages to eliminate
  `install' the db-packages requiring installation"
  (remove (unboxed--db-packages-create))
  (install (unboxed--db-packages-create)))

(defun unboxed--summarize-db-packages-delta (delta)
  "Summarize db-packages-delta  DELTA."
  (when delta
    `(db-packages-delta
      (remove ,(unboxed--summarize-db-packages (unboxed--db-packages-delta-remove delta)))
      (install ,(unboxed--summarize-db-packages (unboxed--db-packages-delta-install delta))))))

;;; this delta is for changes in installed files not involving changes in packages
;;; e.g. byte-compiling libraries or updating the dir info file
(cl-defstruct (unboxed--db-files-delta
	       (:constructor unboxed--db-files-delta-create)
	       (:copier unboxed--db-files-delta-copy))
  "The difference between two sets of installed files.
  Slots:
  `remove' the db-files to eliminate
  `install' the db-files requiring installation"
  (remove (unboxed--db-files-create))
  (install (unboxed--db-files-create)))

(defun unboxed--summarize-source-db-files-delta (delta)
  "Summarize source db-files-delta  DELTA."
  (when delta
    `(db-files-delta
      (remove ,(unboxed--summarize-source-db-files (unboxed--db-files-delta-remove delta)))
      (install ,(unboxed--summarize-source-db-files (unboxed--db-files-delta-install delta))))))


(defun unboxed--summarize-installed-db-files-delta (delta)
  "Summarize installed db-files-delta  DELTA."
  (when delta
    `(db-files-delta
      (remove ,(unboxed--summarize-installed-db-files (unboxed--db-files-delta-remove delta)))
      (install ,(unboxed--summarize-installed-db-files (unboxed--db-files-delta-install delta))))))

(cl-defstruct (unboxed--db-files-transaction
	       (:constructor unboxed--db-files-transaction-create)
	       (:copier unboxed--db-files-transaction-copy))
  "Structure holding data for packages transaction in-progress.
  Slots:
  `db' Database subject to the transaction
  `initial'  The initial files collection as a db-files
  `todo' The changes to be made by the transaction as a db-files-delta
  `done' The changes already made by the transaction as a db-files-delta
  `final' The final files collection as a db-files
  `on-completion' continuation invoked when transaction is complete"
  db
  (initial (unboxed--db-files-create))
  (todo (unboxed--db-files-delta-create))
  (done (unboxed--db-files-delta-create))
  (final (unboxed--db-files-create))
  on-completion)

(defun unboxed--summarize-db-source-files-transaction (txn)
  "Summarize source db-files-transaction TXN."
  (when txn
    `(db-files-transaction
      (db ,(unboxed--area-name (unboxed--sexpr-db-area (unboxed--db-files-transaction-db txn))))
      (initial ,(unboxed--summarize-source-db-files (unboxed--db-files-transaction-initial txn)))
      (todo ,(unboxed--summarize-source-db-files-delta (unboxed--db-files-transaction-todo txn)))
      (done ,(unboxed--summarize-source-db-files-delta (unboxed--db-files-transaction-done txn)))
      (final ,(unboxed--summarize-source-db-files (unboxed--db-files-transaction-final txn)))
      (on-completion ,(and (unboxed--db-files-transaction-on-completion txn) t)))))

(defun unboxed--summarize-db-installed-files-transaction (txn)
  "Summarize source db-files-transaction TXN."
  (when txn
    `(db-files-transaction
      (db ,(unboxed--area-name (unboxed--sexpr-db-area (unboxed--db-files-transaction-db txn))))
      (initial ,(unboxed--summarize-installed-db-files (unboxed--db-files-transaction-initial txn)))
      (todo ,(unboxed--summarize-installed-db-files-delta (unboxed--db-files-transaction-todo txn)))
      (done ,(unboxed--summarize-installed-db-files-delta (unboxed--db-files-transaction-done txn)))
      (final ,(unboxed--summarize-installed-db-files (unboxed--db-files-transaction-final txn)))
      (on-completion ,(and (unboxed--db-files-transaction-on-completion txn) t)))))

(defun unboxed--make-db-files-delta (files-remove files-add)
  "Make a db-files-delta removing FILES-REMOVE and installing FILES-ADD."
  (let ((delta (unboxed--db-files-delta-create))
	ls file files)
    (when (or files-remove files-add)
      (setq ls files-remove
	    files (unboxed--db-files-delta-remove delta))
      (while ls
	(setq file (pop ls))
	(unboxed--add-installed-file-to-db-files files file))
      (setq ls files-add
	    files (unboxed--db-files-delta-install delta))
      (while ls
	(setq file (pop ls))
	(unboxed--add-installed-file-to-db-files files file)))
    delta))

(defun unboxed--make-db-files-transaction (db &optional files-remove files-add on-completion)
  "Make a files-only transaction for database DB.
Arguments:
  `DB' - database subject to transaction
  `FILES-REMOVE' - list of package descriptors to remove from available set
  `FILES-ADD' - list of package descriptor to install into available set
  `ON-COMPLETION' - continuation to invoke after removals and installations
                    are completed"
  (let ((txn (unboxed--packages-transaction-create
	      :db db
	      :on-completion on-completion
	      :initial (unboxed--copy-installed-db-files
			(unboxed--sexpr-db-active db))
	      :todo (unboxed--make-db-files-delta files-remove files-add))))
    txn))
    

(cl-defstruct (unboxed--packages-transaction
	       (:constructor unboxed--packages-transaction-create)
	       (:copier unboxed--packages-transaction-copy))
  "Structure holding data for packages transaction in-progress.
  Slots:
  `db' Database subject to the transaction
  `initial'  The initial packages collection as a db-packages
  `todo' The changes to be made by the transaction as a db-packages-delta
  `done' The changes already made by the transaction as a db-packages-delta
  `final' The final packages collection as a db-packages
  `on-completion' continuation invoked when transaction is complete"
  db
  (initial (unboxed--db-packages-create))
  (todo (unboxed--db-packages-delta-create))
  (done (unboxed--db-packages-delta-create))
  (final (unboxed--db-packages-create))
  on-completion)

(defun unboxed--summarize-db-packages-transaction (txn)
  "Summarize db-packages-transaction TXN."
  (when txn
    `(db-packages-transaction
      (db ,(unboxed--area-name (unboxed--sexpr-db-area (unboxed--packages-transaction-db txn))))
      (initial ,(unboxed--summarize-db-packages (unboxed--packages-transaction-initial txn)))
      (todo ,(unboxed--summarize-db-packages-delta (unboxed--packages-transaction-todo txn)))
      (done ,(unboxed--summarize-db-packages-delta (unboxed--packages-transaction-done txn)))
      (final ,(unboxed--summarize-db-packages (unboxed--packages-transaction-final txn)))
      (on-completion ,(and (unboxed--packages-transaction-on-completion txn) t)))))

(defun unboxed--make-db-packages-delta (pkgs-remove pkgs-add)
  "Make a db-packages-delta removing PKGS-REMOVE and installing PKGS-ADD."
  (let ((delta (unboxed--db-packages-delta-create))
	ls pkg packages)
    (when (or pkgs-remove pkgs-add)
      (setq ls pkgs-remove
	    packages (unboxed--db-packages-delta-remove delta))
      (while ls
	(setq pkg (pop ls))
	(unboxed--add-package-to-db-packages packages pkg))
      (setq ls pkgs-add
	    packages (unboxed--db-packages-delta-install delta))
      (while ls
	(setq pkg (pop ls))
	(unboxed--add-package-to-db-packages packages pkg)))
    delta))

       
(defun unboxed--make-packages-transaction (db &optional pkgs-remove pkgs-add on-completion)
  "Make a transaction for database DB.
Arguments:
  `DB' - database subject to transaction
  `PKGS-REMOVE' - list of package descriptors to remove from available set
  `PKGS-ADD' - list of package descriptor to install into available set
  `ON-COMPLETION' - continuation to invoke after removals and installations
                    are completed"
  (let ((txn (unboxed--packages-transaction-create
	      :db db
	      :on-completion on-completion
	      :initial (unboxed--sexpr-db-available db)
	      :todo (unboxed--make-db-packages-delta pkgs-remove pkgs-add))))
    txn))
    


(cl-defstruct (unboxed--transaction
	       (:constructor unboxed--transaction-create)
	       (:copier unboxed--transaction-copy))
  "Structure holding data for database transaction in-progress.
  Slots:
  `db' Database subject to the transaction
  `initial'  The initial database state as a transaction-state
  `todo' The changes to be made by the transaction as a transaction-delta
  `done' The changes already made by the transaction as a transaction-delta
  `final' The final database state as a transaction-state
  `on-completion' continuation invoked when transaction is complete"
  db
  (initial (unboxed--db-state-create))
  (todo (unboxed--db-delta-create))
  (done (unboxed--db-delta-create))
  (final (unboxed--db-state-create))
  on-completion)

(defun unboxed--summarize-db-transaction (txn)
  "Summarize db-transaction TXN."
  (when txn
    `(db-transaction
      (db ,(unboxed--area-name (unboxed--sexpr-db-area (unboxed--transaction-db txn))))
      (initial ,(unboxed--summarize-db-state (unboxed--transaction-initial txn)))
      (todo ,(unboxed--summarize-db-delta (unboxed--transaction-todo txn)))
      (done ,(unboxed--summarize-db-delta (unboxed--transaction-done txn)))
      (final ,(unboxed--summarize-db-state (unboxed--transaction-final txn)))
      (on-completion ,(and (unboxed--transaction-on-completion txn) t)))))

(defun unboxed--make-db-delta (pkgs-remove pkgs-add)
  "Make a transaction-delta removing PKGS-REMOVE and installing PKGS-ADD."
  (let ((delta (unboxed--db-delta-create))
	ls pkg state)
    (when (or pkgs-remove pkgs-add)
      (setq ls pkgs-remove
	    state (unboxed--db-delta-remove delta))
      (while ls
	(setq pkg (pop ls))
	(unboxed--add-package-to-db-state state pkg))
      (setq ls pkgs-add
	    state (unboxed--db-delta-install delta))
      (while ls
	(setq pkg (pop ls))
	(unboxed--add-package-to-db-state state pkg)))
    delta))

       
(defun unboxed--make-transaction (db &optional pkgs-remove pkgs-add on-completion)
  "Make a transaction for database DB.
Arguments:
  `DB' - database subject to transaction
  `PKGS-REMOVE' - list of package descriptors to remove from active set
  `PKGS-ADD' - list of package descriptor to install into active set
  `ON-COMPLETION' - continuation to invoke after removals and installations
                    are completed"
  (let ((txn (unboxed--transaction-create
	      :db db
	      :on-completion on-completion
	      :initial (unboxed--copy-db-state
			(unboxed--sexpr-db-active db))
	      :todo (unboxed--make-db-delta pkgs-remove pkgs-add))))
    txn))
    

;; note - it's entirely possible for a site to have one version of unboxed installed
;; and for a user to have another version installed.  Therefore, we record
;; the layout of structures in structure itself to allow some forward/backward
;; compatibility - eventually
(cl-defstruct (unboxed--sexpr-db
	       (:constructor unboxed--sexpr-db-create)
	       (:copier unboxed--sexpr-db-copy))
  "Structure holding the tables of data for unboxed in sexpr db representation.
The available db state may include multiple versions of a package, or
incompatible packages. The active db state includes only those packages
available for loading.
   Slots:
   `layouts' Association list of data structure layouts used in this db
   `areas' Association list of area structs in scope for dependency calculations
   `area' area struct for this database
   `available' db packages for all boxed packages of area on disk
   `active' db state for all boxed packages of area available for package loading"
  layouts
  areas
  area
  (available (unboxed--db-packages-create))
  (active (unboxed--db-state-create)))

(defun unboxed--sexpr-db-name (db)
  "Return the name of DB."
  (unboxed--area-name
   (unboxed--sexpr-db-area db)))

(defun unboxed--sexpr-db-boxes (db)
  "Return the box paths of DB."
  (unboxed--area-boxes
   (unboxed--sexpr-db-area db)))

(defun unboxed--sexpr-db-path (db)
  "Return the path to the file for DB."
  (unboxed--area-db-path
   (unboxed--sexpr-db-area db)))

(defun unboxed--summarize-sexpr-db (db)
  "Summarize database DB."
  (when db
    `(sexpr-db
      (layouts ,(and (unboxed--sexpr-db-layouts db) t))
      (areas ,(mapcar #'car (unboxed--sexpr-db-areas db)))
      (area ,(unboxed--summarize-area (unboxed--sexpr-db-area db)))
      (available ,(let ((q (make-queue)))
		    (maphash (lambda (id pq) (queue-enqueue q `(,id ,(queue-length pq))))
			     (unboxed--db-packages-descs (unboxed--sexpr-db-available db)))
		    (queue-all q)))
      (active ,(unboxed--summarize-db-state
		(unboxed--sexpr-db-active db))))))

  
;; (defun unboxed--sexpr-db-datadir-patterns (db)
;;   "Return the  of DB"
;;   (unboxed--area-datadir-pats
;;    (unboxed--sexpr-db-area db)))

(defun unboxed--sexpr-db-categories (db)
  "Return the file categories of DB."
  (unboxed--area-categories
   (unboxed--sexpr-db-area db)))

(defun unboxed--sexpr-db-category-location (db catname)
  "Return the location category CATNAME of DB."
  (let ((cats (unboxed--sexpr-db-categories db))
	result)
    (setq result (assq catname cats)
	  result (and result
		      (unboxed-file-category-location (cdr result))))
    result))

(defun unboxed--area-category-location (area catname)
  "Return the location category CATNAME of AREA."
  (let ((cats (unboxed--area-categories area))
	result)
    (setq result (assq catname cats)
	  result (and result
		      (unboxed-file-category-location (cdr result))))
    result))

(defun unboxed--area-category (area cat-name)
  "Return the category CAT-NAME of AREA."
  (let ((result (assq cat-name (unboxed--area-categories area))))
    (and result (cdr result))))

(cl-defstruct (unboxed-file-category
               (:constructor unboxed-file-category-create)
	       (:copier unboxed-file-category-copy))
  "Structure for contents of package and each is installed.
Other than predicate, the function slots may be nil.
  Slots:
  `name' name of file category as symbol
  `area' name of the area using this category definition
  `path-variable' elisp variable for path associated with this
         file category, nil if none
  `location' path for installing this file category
  `libraries' list of absolute library paths that must be loaded
              for unboxing operations"
  name
  area
  path-variable
  location
  libraries)


(defun unboxed--summarize-file-category (cat)
  "Construct non-recursive summary of category CAT."
  `(file-category
    (name ,(unboxed-file-category-name cat))
    (area ,(if (symbolp (unboxed-file-category-area cat))
	       `',(unboxed-file-category-area cat)
	     (unboxed--area-name (unboxed-file-category-area cat))))
    (path-variable ,(unboxed-file-category-path-variable cat))
    (predicate ,(and (unboxed-file-category-predicate cat) t))
    (location ,(unboxed-file-category-location cat))
    (install-files ,(and (unboxed-file-category-install-files cat) t))
    (finalize-install-files ,(and (unboxed-file-category-finalize-install-files cat) t))
    (remove-files ,(and (unboxed-file-category-remove-files cat) t))
    (finalize-remove-files ,(and (unboxed-file-category-finalize-remove-files cat) t))))
     
(cl-defstruct (unboxed-source-file
               (:constructor unboxed-source-file-create)
	       (:copier unboxed-source-file-struct-copy))
  "Structure for a file that exists in the boxed package directory tree.
  Slots:
  `id' symbol used as unique key for package + file
  `package-desc' unboxed-package-desc of package containing the file
  `db-category' unboxed-file-category to which the file belongs
  `file' location of file relative to package box directory"
  id
  package-desc
  db-category
  file)


(cl-defstruct (unboxed--Csource-file
               (:constructor unboxed--Csource-file-create)
	       (:copier unboxed--Csource-file-struct-copy))
  "Concrete source-file structure for async operations.
  Slots:
  `id' symbol used as unique key for package + file
  `package' package id (versioned)
  `category' name of unboxed-file-category to which the file belongs
  `file' location of file relative to package box directory"
  id
  package
  category
  file)

(defun unboxed--summarize-source-file (src)
  "Summarize source file SRC."
  `(source-file
    (id ,(unboxed-source-file-id src))
    (pkg ,(unboxed-package-desc-name (unboxed-source-file-package-desc src)))
    (cat ,(unboxed-file-category-name (unboxed-source-file-db-category src)))
    (file ,(unboxed-source-file-file src))))

(cl-defstruct (unboxed-installed-file
               (:constructor unboxed-installed-file-create)
	       (:copier unboxed-installed-file-struct-copy))
  "Structure for a file that is installed for a package.
An installed-file record may be created even if the installation of
the file failed, so that the messages/warnings/log will be kept
for reference.
  Slots:
  `source' source-file from which this is derived
  `id' symbol used as unique key for category + file
  `file' location of file relative to category-location
         may not be identical to source file, or even have the same
         base name, e.g. byte-compiled files
         Stored as symbol
  `package-desc' unboxed-package-desc of package file derives from
  `db-category' unboxed-file-category to which the file belongs
  `created' boolean indicated whether installing this file succeeded
  `log' Any relevant data generated during the installation process
        for this specific file
  `warnings' *Warnings* buffer during install process
  `messages' *Messages* buffer during install process"
  source
  id
  file
  package-desc
  db-category
  created
  log
  warnings
  messages)

(cl-defstruct (unboxed--Cinstalled-file
               (:constructor unboxed--Cinstalled-file-create)
	       (:copier unboxed--Cinstalled-file-struct-copy))
  "Concrete form of installed-file for async operations.
  Slots:
  `source' id of concrete source file
  'package' id of concrete package-desc
  `id' symbol used as unique key for category + file
  `file' location of file relative to category-location
         may not be identical to source file, or even have the same
         base name, e.g. byte-compiled files
         Stored as symbol
  `category' category name of file
  `created' boolean indicated whether installing this file succeeded
  `log' Any relevant data generated during the installation process
        for this specific file
  `warnings' *Warnings* buffer during install process
  `messages' *Messages* buffer during install process"
  source
  package
  id
  file
  category
  created
  log
  warnings
  messages)

(defun unboxed--summarize-installed-file (inst)
  "Summarize installed file INST."
  `(installed-file
    (source
     ,(unboxed--file-id (unboxed-installed-file-source inst)))
    (id ,(unboxed-installed-file-id inst))
    (file ,(unboxed-installed-file-file inst))
    (created ,(unboxed-installed-file-created inst))
    (log ,(and (unboxed-installed-file-log inst) t))
    (warnings ,(and (unboxed-installed-file-warnings inst) t))
    (messages ,(and (unboxed-installed-file-messages inst) t))))

(defun unboxed--concretize-installed-file (inst)
  "Summarize installed file INST."
  (unboxed--Cinstalled-file-create
   :source (unboxed--file-id (unboxed-installed-file-source inst))
   :package (unboxed-package-desc-id (unboxed-installed-file-package inst))
   :id (unboxed-installed-file-id inst)
   :file (unboxed-installed-file-file inst)
   :created (unboxed-installed-file-created inst)
   :log (unboxed-installed-file-log inst)
   :warnings (unboxed-installed-file-warnings inst)
   :messages (unboxed-installed-file-messages inst)))

(cl-defstruct (unboxed--struct-layout
               (:constructor unboxed--struct-layout-create)
	       (:copier unboxed--struct-layout-copy))
  "Record of struct layout for instantiating structs
from a file.
  Slots:
  `version' version of this struct
  `seq-type' Value of (cl-struct-sequence-type 'package-desc)
  `keys' Keywords for use with constructor for the slot at
         the corresponding index
  `slot-info' Value of (cl-struct-slot-info 'pacakge-desc)"
  (version 1 :read-only t)
  seq-type
  keys
  slot-info)

(cl-defstruct (unboxed-package-desc
               (:constructor unboxed-package-desc-create)
	       (:copier unboxed-package-desc-copy)
	       (:include package-desc))
  "Package desc structure extended with fields recording its
installation manager.
  Slots:
  `db' unboxed database that owns this package-desc
  `id' symbol that is unique for package name + version string
  `single' boolean which is t if the package is for a single library file
  `simple' boolean which is t if the package directory has no subdirectories
  `version-string' version string for this package
  `manager' name of installation manager for this package
  `files' unboxed--db-files collection of source files in the package box"
  db
  id
  single
  simple
  (version-string "0")
  (manager 'package)
  files)

(cl-defstruct (unboxed-Cpackage-desc
               (:constructor unboxed-Cpackage-desc-create)
	       (:copier unboxed-Cpackage-desc-copy))
  "Concrete form of unboxed-package-desc.
  Slots:
  `name' package name
  `id' symbol that is unique for package name + version string
  `area' area containing the package
  `dir' path to boxed files
  `single' boolean which is t if the package is for a single library file
  `simple' boolean which is t if the package directory has no subdirectories
  `version-string' version string for this package
  `manager' name of installation manager for this package
  `files' unboxed--db-files collection of source files in the package box"
  name
  id
  area
  dir
  single
  simple
  (version-string "0")
  (manager 'package)
  files)

(defun unboxed--summarize-package-desc (pd)
  "Summarize unboxed package descriptor PD."
  (when pd
    `(unboxed-package-desc
      (db ,(unboxed--area-name (unboxed-package-desc-area pd)))
      (id ,(unboxed-package-desc-id pd))
      (single ,(unboxed-package-desc-single pd))
      (simple ,(unboxed-package-desc-simple pd))
      (version-string ,(unboxed-package-desc-version-string pd))
      (manager ,(unboxed-package-desc-manager pd))
      (files ,(unboxed--summarize-source-db-files (unboxed-package-desc-files pd))))))
  
(defun unboxed-package-desc-area (pd)
  "Area of package descriptor PD."
  (unboxed--sexpr-db-area
   (unboxed-package-desc-db pd)))

(defun unboxed-package-desc-areas (pd)
  "Areas of package descriptor PD."
  (unboxed--sexpr-db-areas
   (unboxed-package-desc-db pd)))

(defun unboxed--make-package-desc-id (pd)
  "Construct identifier of package descriptor PD."
  (let ((name (unboxed-package-desc-name pd))
	(vs (unboxed-package-desc-version-string pd)))
    (intern (concat (symbol-name name) "#" vs))))

(defun unboxed--make-file-id (inst-or-src)
  "Construct identifier of installed file INST."
  (let ((name (unboxed--file-file inst-or-src))
	(pkg (unboxed--file-package inst-or-src)))
    (intern (format "%s#%s" pkg name))))

(defun unboxed--make-source-file-id (src)
  "Construct identifier of source file SRC."
  (let ((name (symbol-name (unboxed-source-file-file src)))
	(vs (unboxed-source-file-version src))
	(pkg (symbol-name (unboxed-source-file-package src))))
    (intern (concat pkg "#" vs "@" name))))

(defun unboxed-package-single-p (pd)
  "Test whether package descriptor PD is for a single file package."
  (let ((d (package-desc-dir pd))
	(name (symbol-name (package-desc-name pd)))
	re-name all main auto pkg r)
    (when (and d (file-accessible-directory-p d))
      (setq re-name (regexp-quote name)
	    all (directory-files d nil "^[^.].*$")
	    main (directory-files d nil (concat "^" re-name "\\.elc?$"))
	    auto (directory-files d nil (concat "^" re-name "-autoloads\\.elc?$"))
	    pkg (directory-files d nil (concat "^" re-name "-pkg\\.elc?$")))
      (when (= (length all) (+ (length main) (length auto) (length pkg)))
	(setq r t)))
    r))


(defun unboxed-package-simple-p (pd)
  "Test whether package descriptor PD is for a package with no subdirectories."
  (let ((d (package-desc-dir pd))
	(no-subdirs t)
	all fn)
    (setq all (directory-files d t "^[^.].*$"))
    (while (and no-subdirs all)
      (setq fn (pop all)
	    no-subdirs (not (file-directory-p fn))))
    no-subdirs))

(defun unboxed-package-any-p (_pd)
  "Test that succeeds for any package descriptor PD."
  ;; shut up byte-compiler
  t)

(defun unboxed-package-none-p (_pd)
  "Test that fails for any package descriptor PD."
  ;; shut up byte-compiler
  nil)

(defun unboxed-source-file-version (src)
  "Get version string of package containing source file SRC."
  (unboxed-package-desc-version-string
   (unboxed-source-file-package-desc src)))

(defun unboxed-source-file-package-location (src)
  "Get location of package containing source file SRC."
  (unboxed-package-desc-dir
   (unboxed-source-file-package-desc src)))

(defun unboxed-source-file-package (src)
  "Get name of package containing source file SRC."
  (unboxed-package-desc-name
   (unboxed-source-file-package-desc src)))
   
(defun unboxed-source-file-category (src)
  "Get category name of package containing source file SRC."
  (unboxed-file-category-name
   (unboxed-source-file-db-category src)))
   
(defun unboxed-source-file-category-location (src)
  "Get category location of package containing source file SRC."
  (unboxed-file-category-location
   (unboxed-source-file-db-category src)))

(defun unboxed-installed-file-version (inst)
  "Get version string of package containing installed file INST."
  (unboxed-package-desc-version-string
   (unboxed-installed-file-package-desc inst)))

(defun unboxed-installed-file-package-location (inst)
  "Get location of package containing installed file INST."
  (unboxed-package-desc-dir
   (unboxed-installed-file-package-desc inst)))

(defun unboxed-installed-file-package (inst)
  "Get name of package containing installed file INST."
  (unboxed-package-desc-name
   (unboxed-installed-file-package-desc inst)))
   
(defun unboxed-installed-file-category (inst)
  "Get name of category containing installed file INST."
  (unboxed-file-category-name
   (unboxed-installed-file-db-category inst)))
   
(defun unboxed-installed-file-category-location (inst)
  "Get location of category containing installed file INST."
  (unboxed-file-category-location
   (unboxed-installed-file-db-category inst)))
   
(defun unboxed--make-category-queue-aqueue (cats)
  "Make an aqueue of queues for category set CATS."
  (let ((q (make-queue))
	(ls cats)
	c-pr)
    (while ls
      (setq c-pr (pop ls))
      (queue-enqueue q
		     `(,(unboxed-file-category-name (cdr c-pr))
		       .
		       ,(ajq--make-queue))))
    q))

(defun unboxed--exists-source-file-in-cat-queue-p (src aq)
  "Test whether source-file with same category and file as SRC exists in aqueue AQ."
  (let ((cat (unboxed-source-file-category src))
	(file (unboxed-source-file-file src))
	pr)
    (setq pr (assq cat (queue-all aq)))
    ;; (unless pr
    ;;   (signal 'unboxed-invalid-category `(,inst ,als)))
    (when pr
      (seq-some (cdr pr)
		(lambda (src1)
		  (eq file
		      (unboxed-source-file-file src1)))))))
      
(defun unboxed--exists-installed-file-in-cat-queue-p (inst aq)
  "Test whether INST exists in category aqueue AQ."
  (let ((cat (unboxed-installed-file-category inst))
	(file (unboxed-installed-file-file inst))
	pr)
    (setq pr (assq cat (queue-all aq)))
    ;; (unless pr
    ;;   (signal 'unboxed-invalid-category `(,inst ,als)))
    (when pr
      (seq-some (cdr pr)
		(lambda (inst)
		  (eq file
		      (unboxed-installed-file-file inst)))))))
      

(defun unboxed--add-source-file-to-cat-queue (aq src)
  "Add src-file SRC to aqueue AQ."
  ;; (message "Adding \n%s\n to \n%s"
  ;; 	   (pp (unboxed--summarize-
  (let ((cat (unboxed-source-file-category src))
	cat-name pr)
    (setq cat-name cat ;(unboxed-file-category-name cat)
	  pr (assq cat-name (queue-all aq)))
    ;; (unless pr
    ;;   (display-warning :error (format "Category not found %S" cat-name)))
    ;; (unless pr
    ;;   (signal 'unboxed-invalid-category `(,inst ,als)))
    (unless pr
      (setq pr (cons cat-name (make-queue)))
      (queue-enqueue aq pr))
    (queue-enqueue (cdr pr) src))
  aq)

;; for use in a package-specific set of source files
(defun unboxed--add-source-file (sources src)
  "Add source file SRC to hash table SOURCES by its package relative path."
  (let ((k (unboxed-source-file-file src))
	q)
    (setq q (or (gethash k sources)
		(puthash k (make-queue) sources)))
    (queue-enqueue q src))
  sources)

;; for use in db-specific set of source files
(defun unboxed--add-source-file-id (sources src)
  "Add source file SRC to hash table SOURCES by its identity."
  (let ((k (unboxed-source-file-file src))
	q)
    (setq q (or (gethash k sources)
		(puthash k (make-queue) sources)))
    (queue-enqueue q src))
  sources)

(defun unboxed--add-installed-file-to-cat-queue (aq inst)
  "Add installed-file INST to aqueue AQ."
  (let ((cat (unboxed-installed-file-category inst))
	pr)
    (setq pr (assq cat (queue-all aq)))
    ;; (unless pr
    ;;   (signal 'unboxed-invalid-category `(,inst ,als)))
    (unless pr
      (setq pr (cons cat (make-queue)))
      (queue-enqueue aq pr))
    (queue-enqueue (cdr pr) inst))
  aq)

(defun unboxed--get-cat-queue (aq catname)
  "Return queue asociated with CATNAME in association-queue AQ."
  (let ((q (assq catname (queue-all aq))))
    (when q
      (setq q (cdr q)))
    q))

(defun unboxed--add-installed-file-source (sources inst)
  "Add installed file INST to hash table SOURCES by its source path."
  (let ((k (unboxed-installed-file-source inst))
	q)
    (setq q (or (gethash k sources)
		(puthash k (make-queue) sources)))
    (queue-enqueue q inst))
  sources)

(defun unboxed--add-installed-file (installed inst)
  "Add installed file INST to hash table INSTALLED by its standard id."
  (let ((k (unboxed-installed-file-id inst))
	q)
    (setq q (or (gethash k installed)
		(puthash k (make-queue) installed)))
    (queue-enqueue q inst))
  installed)

(defun unboxed--remove-installed-file (installed inst)
  "Remove installed file INST from hash table INSTALLED by its standard id."
  (remhash (unboxed-installed-file-id inst) installed)
  installed)

(defun unboxed--remove-installed-file-from-cat-queue (aq inst)
  "Add installed-file INST to aqueue AQ."
  (let ((cat (unboxed-installed-file-category inst))
	pr q)
    (setq pr (assq cat (queue-all aq)))
    ;; (unless pr
    ;;   (signal 'unboxed-invalid-category `(,inst ,als)))
    (when pr
      (setq q (cdr pr)))
    (queue-remove q inst)
    aq))


(defun unboxed--add-installed-file-to-db-files (files inst)
  "Add INST to FILES."
  (unboxed--add-installed-file-to-cat-queue
   (unboxed--db-files-locations files)
   inst)
  (unboxed--add-installed-file
   (unboxed--db-files-files files)
   inst))

(defun unboxed--remove-installed-file-from-db-files (files inst)
  "Remove INST to FILES."
  (unboxed--remove-installed-file-from-cat-queue
   (unboxed--db-files-locations files)
   inst)
  (unboxed--remove-installed-file
   (unboxed--db-files-files files)
   inst))

(defun unboxed--add-source-file-to-db-files (files src)
  "Add SRC to FILES."
  (let ((ls (unboxed--db-files-locations files))
	(fs (unboxed--db-files-files files)))
    ;; (message "Adding \n%s\n to locations\n%s"
    ;; 	     (pp (unboxed--summarize-source-file src))
    ;; 	     (pp (unboxed--summarize-source-file-cat-queue ls)))
    (unboxed--add-source-file-to-cat-queue
     ls
     src)
    ;; (message "Added \n%s\n to locations\n%s"
    ;; 	     (pp (unboxed--summarize-source-file src))
    ;; 	     (pp (unboxed--summarize-source-file-cat-queue ls)))
    (unboxed--add-source-file
     fs
     src)))

(defun unboxed--add-package-name (packages pd)
  "Add package descriptor PD to hash table PACKAGES by its name."
  (let ((k (unboxed-package-desc-name pd))
	q)
    (setq q (or (gethash k packages)
		(puthash k (make-queue) packages)))
    (queue-enqueue q pd))
  packages)

(defun unboxed--add-package-id (packages pd)
  "Add package descriptor PD to hash table PACKAGES by its name."
  (let ((k (unboxed-package-desc-id pd))
	q)
    (setq q (or (gethash k packages)
		(puthash k (make-queue) packages)))
    (queue-enqueue q pd))
  packages)

(defun unboxed--remove-package-name (packages pd)
  "Remove package descriptor PD from hash table PACKAGES by its name."
  (let ((k (unboxed-package-desc-name pd))
	q)
    (setq q (gethash k packages))
    (when q
      (queue-remove q pd)))
  packages)

(defun unboxed--remove-package-id (packages pd)
  "Remove package descriptor PD to hash table PACKAGES by its name."
  ;; There should never be a queue with more or less than 1 element
  ;; assigned to a versioned package identifier
  (remhash (unboxed-package-desc-id pd) packages)
  packages)


(defun unboxed--make-source-file (pd cat file)
  "Make installed-file structure.
Arguments:
  PD - unboxed package descriptor
  CAT - category structure
  FILE - file in package's boxed location"
  (let ((src
	 (unboxed-source-file-create :package-desc pd
				     :db-category cat
				     :file (intern file))))
    (setf (unboxed-source-file-id src)
	  (unboxed--make-source-file-id src))
    src))

(defun unboxed--file-file (inst-or-src)
  "Get the file path associated with INST-OR-SRC."
  (when inst-or-src
    (cond
     ((unboxed-source-file-p inst-or-src)
      (unboxed-source-file-file inst-or-src))
     ((unboxed-installed-file-p inst-or-src)
      (unboxed-installed-file-file inst-or-src))
     ((unboxed--Csource-file-p inst-or-src)
      (unboxed--Csource-file-file inst-or-src))
     ((unboxed--Cinstalled-file-p inst-or-src)
      (unboxed--Cinstalled-file-file inst-or-src))
     (t
      (signal 'unboxed-invalid-file-record inst-or-src)))))

(defun unboxed--file-package (inst-or-src)
  "Get the package id associated with INST-OR-SRC."
  (when inst-or-src
    (cond
     ((unboxed-source-file-p inst-or-src)
      (unboxed-package-desc-id
       (unboxed-source-file-package-desc inst-or-src)))
     ((unboxed-installed-file-p inst-or-src)
      (unboxed-package-desc-id
       (unboxed-installed-file-package-desc inst-or-src)))
     ((unboxed--Csource-file-p inst-or-src)
      (unboxed--Csource-file-package inst-or-src))
     ((unboxed--Cinstalled-file-p inst-or-src)
      (unboxed--Cinstalled-file-package inst-or-src))
     (t
      (signal 'unboxed-invalid-file-record inst-or-src)))))

(defun unboxed--file-package-desc (inst-or-src)
  "Get the unboxed-package-desc associated with INST-OR-SRC."
  (when inst-or-src
    (cond
     ((unboxed-source-file-p inst-or-src)
      (unboxed-source-file-package-desc inst-or-src))
     ((unboxed-installed-file-p inst-or-src)
      (unboxed-installed-file-package-desc inst-or-src))
     (t
      (signal 'unboxed-invalid-file-record inst-or-src)))))

(defun unboxed--file-cat (inst-or-src)
  "Get the file-category name associated with INST-OR-SRC."
  (when inst-or-src
    (cond
     ((unboxed-source-file-p inst-or-src)
      (unboxed-source-file-category inst-or-src))
     ((unboxed-installed-file-p inst-or-src)
      (unboxed-installed-file-category inst-or-src))
     ((unboxed--Csource-file-p inst-or-src)
      (unboxed--Csource-file-category inst-or-src))
     ((unboxed--Cinstalled-file-p inst-or-src)
      (unboxed--Cinstalled-file-category inst-or-src))
     (t
      (signal 'unboxed-invalid-file-record inst-or-src)))))

(defun unboxed--file-db-category (inst-or-src)
  "Get the file-category structure associated with INST-OR-SRC."
  (when inst-or-src
    (cond
     ((unboxed-source-file-p inst-or-src)
      (unboxed-source-file-db-category inst-or-src))
     ((unboxed-installed-file-p inst-or-src)
      (unboxed-installed-file-db-category inst-or-src))
     (t
      (signal 'unboxed-invalid-file-record inst-or-src)))))

(defun unboxed--file-id (inst-or-src)
  "Get the identifier associated with INST-OR-SRC."
  (when inst-or-src
    (cond
     ((unboxed-source-file-p inst-or-src)
      (unboxed-source-file-id inst-or-src))
     ((unboxed-installed-file-p inst-or-src)
      (unboxed-installed-file-id inst-or-src))
     ((unboxed--Csource-file-p inst-or-src)
      (unboxed--Csource-file-id inst-or-src))
     ((unboxed--Cinstalled-file-p inst-or-src)
      (unboxed--Cinstalled-file-id inst-or-src))
     (t
      (signal 'unboxed-invalid-file-record inst-or-src)))))

(defun unboxed--file-source (inst)
  "Get the source file associated with INST."
  (when inst
    (cond
     ((unboxed-installed-file-p inst)
      (unboxed--file-file
       (unboxed-installed-file-source inst)))
     ((unboxed--Cinstalled-file-p inst)
      (unboxed--Cinstalled-file-source inst))
     (t
      (signal 'unboxed-invalid-file-record inst)))))


(defun unboxed--file-log (inst)
  "Get the log text associated with INST."
  (when inst
    (cond
     ((unboxed-installed-file-p inst)
      (unboxed-installed-file-log inst))
     ((unboxed--Cinstalled-file-p inst)
      (unboxed--Cinstalled-file-log inst))
     (t
      (signal 'unboxed-invalid-file-record inst)))))

(defun unboxed--file-warnings (inst)
  "Get the warnings text associated with INST."
  (when inst
    (cond
     ((unboxed-installed-file-p inst)
      (unboxed-installed-file-warnings inst))
     ((unboxed--Cinstalled-file-p inst)
      (unboxed--Cinstalled-file-warnings inst))
     (t
      (signal 'unboxed-invalid-file-record inst)))))

(defun unboxed--file-messages (inst)
  "Get the messages text associated with INST."
  (when inst
    (cond
     ((unboxed-installed-file-p inst)
      (unboxed-installed-file-messages inst))
     ((unboxed--Cinstalled-file-p inst)
      (unboxed--Cinstalled-file-messages inst))
     (t
      (signal 'unboxed-invalid-file-record inst)))))

(defun unboxed--set-file-log (inst val)
  "Set the log text associated with INST."
  (when inst
    (cond
     ((unboxed-installed-file-p inst)
      (setf (unboxed-installed-file-log inst) val))
     ((unboxed--Cinstalled-file-p inst)
      (setf (unboxed--Cinstalled-file-log inst) val))
     (t
      (signal 'unboxed-invalid-file-record inst)))))

(defun unboxed--set-file-warnings (inst val)
  "Set the warning text associated with INST."
  (when inst
    (cond
     ((unboxed-installed-file-p inst)
      (setf (unboxed-installed-file-warnings inst) val))
     ((unboxed--Cinstalled-file-p inst)
      (setf (unboxed--Cinstalled-file-warnings inst) val))
     (t
      (signal 'unboxed-invalid-file-record inst)))))

(defun unboxed--set-file-messages (inst val)
  "Set the warning text associated with INST."
  (when inst
    (cond
     ((unboxed-installed-file-p inst)
      (setf (unboxed-installed-file-messages inst) val))
     ((unboxed--Cinstalled-file-p inst)
      (setf (unboxed--Cinstalled-file-messages inst) val))
     (t
      (signal 'unboxed-invalid-file-record inst)))))

(defun unboxed--make-installed-file (src dst-file &optional cat pkg)
  "Make installed-file structure.
Arguments:
  SRC - file record
  DST-FILE - file relative to unboxing category location
  CAT - db file-category to which this belongs
        Default is the category of SRC
  PKG - unboxed-package-desc of package to which this belongs
        Default is the package of SRC"
  (let ((inst
	 (unboxed-installed-file-create
	  :source src
	  :file dst-file
	  :db-category (or cat
			   (unboxed--file-db-category src))
	  :package-desc (or pkg
			   (unboxed--file-package-desc src)))))
    (setf (unboxed-installed-file-id inst)
	  (unboxed--make-file-id inst))
    inst))

(defun unboxed--make-Cinstalled-file (src dst-file &optional cat pkg)
  "Make installed-file structure.
Arguments:
  SRC - file record
  DST-FILE - file relative to unboxing category location
  CAT - file category name of installed file
        Default is the category of SRC
  PKG - id of package of installed file
        Default is the package of SRC"
  (let ((inst
	 (unboxed--Cinstalled-file-create
	  :source src
	  :file dst-file
	  :category (or cat
			(unboxed--file-cat src))
	  :package (or pkg
		       (unboxed--file-package src)))))
    (setf (unboxed-installed-file-id inst)
	  (unboxed--make-file-id inst))
    inst))

(defun unboxed--import-Cinstalled-file (db c-inst)
  "Convert concrete installed file into internal db installed-file.
Arguments:
  DB - database to import into
  C-INST - concrete installed-file to import"
  (let ((pd (unboxed--get-available-package-id
	     db
	     (unboxed--file-package c-inst)))
	(cat (unboxed--get-cat-from-db-by-name
	      db 
	      (unboxed--file-cat c-inst)))
	src)
    (setq src (unboxed--get-source-file-from-package
	       pd
	       (unboxed--make-file-id
		(unboxed--file-source c-inst))))
    (unboxed--make-installed-file
     src
     (unboxed--file-file c-inst)
     cat
     pd)))


(defun unboxed--add-source-file-to-package-desc (pd cat fn)
  "Add source file FN in category CAT to package desc PD."
  ;; (message "Adding %S to \n%s\n in \n%s"
  ;; 	   fn
  ;; 	   (pp (unboxed--summarize-package-desc pd))
  ;; 	   (pp (unboxed--summarize-file-category cat)))
  (let ((db-files (unboxed-package-desc-files pd)))
    (when (null db-files)
      (setq db-files (unboxed--db-files-create))
      (setf (unboxed-package-desc-files pd) db-files))
    (unboxed--add-source-file-to-db-files
     db-files
     (unboxed--make-source-file pd cat fn))))

(defun unboxed--add-source-files-to-package-desc (pd als cats)
  "Add category alist ALS of paths to package desc PD and categories CATS."
    (let ((db-files (unboxed-package-desc-files pd))
	  pr c ls cat)
      (when (not (null db-files))
	;; discard any existing files
	(message "Replacing existing source files for %s"
		 (unboxed-package-desc-id pd))
	(setq db-files (unboxed--db-files-create))
	(setf (unboxed-package-desc-files pd) db-files))
    (while als
      (setq pr (pop als)
	    c (car pr)
	    ls (cdr pr)
	    cat (assq c cats))
      ;; (message "Adding \n%S\n to \n%s"
      ;; 	       (pp als)
      ;; 	       (pp (unboxed--summarize-package-desc pd)))
      (unless cat
	(display-warning 'error (format "Category name %s not recognized, ignoring %S" c ls)))
      (when cat
	(setq cat (cdr cat))
	(mapc (lambda (path) (unboxed--add-source-file-to-package-desc pd cat path)) ls)))
    pd))

(defun unboxed--add-source-files-to-packages (pds pkg-alss cats)
  "Add source files from PKG-ALSS to package descriptors PDS.
Classified by categories CATS."
  (let (pd id als)
    (while pds
      (setq pd (pop pds)
	    id (unboxed-package-desc-id pd)
	    als (assq id pkg-alss))
      ;; (message "Adding %S\n%s" id (pp als))
      (if (null als)
	  (display-warning :error (format "Missing results for %S" id))
	(setq als (cdr als))
	(unboxed--add-source-files-to-package-desc pd als cats)))))
    

  
(defun unboxed--catalog-package-desc-files (pd)
  "Construct source files for package PD."
  ;; (message "Cataloging files for %s" (unboxed--summarize-package-desc pd))
  (let ((db (unboxed-package-desc-db pd))
	(pd-files (unboxed-package-desc-files pd))
	(pkg-dir (file-name-as-directory (unboxed-package-desc-dir pd)))
	area cats cat ls cat-pred files  noncat-files N)
    (unless pd-files
      (setq pd-files (unboxed--db-files-create))
      (setf (unboxed-package-desc-files pd) pd-files))
    (setq area (unboxed--sexpr-db-area db)
	  cats (unboxed--area-categories area)
	  ls cats
	  N (length pkg-dir)
	  files (mapcar (lambda (fn) (substring fn N))
			(directory-files-recursively pkg-dir "")))
    (while ls
      (setq cat (cdr (pop ls))
	    noncat-files (make-queue)
	    cat-pred (unboxed-file-category-predicate cat))
      (mapc (lambda (fn)
	      (if (funcall cat-pred fn)
		  (unboxed--add-source-file-to-db-files
		   pd-files
		   (unboxed--make-source-file pd cat fn))
		(queue-enqueue noncat-files fn)))
	    files)
      (setq files (queue-all noncat-files)))
    pd-files))

(defun unboxed--catalog-packages (pkgs cat-preds)
  "Classify files of packages PKGS according to CAT-PREDS.
PKGS is an alist mapping package symbols to their boxed directory."
  (message "Catalog-packages\n%s" (pp pkgs))
  (let ((q (make-queue))
	pr pkg pkg-dir)
    (while pkgs
      (setq pr (pop pkgs)
	    pkg (car pr)
	    pkg-dir (cdr pr))
      (queue-enqueue q `(,pkg . ,(unboxed--catalog-package-files pkg pkg-dir cat-preds))))
    ;; (message "Found %S" q)
    (queue-all q)))

(defun unboxed--catalog-package-files (_pkg pkg-dir cat-preds)
  "Classify package PKG files in PKG-DIR according to CAT-PREDS."
  ;; (message "Cataloging files for %s in %s" pkg pkg-dir)
  (setq pkg-dir (file-name-as-directory pkg-dir))
  (let ((cq (make-queue))
	pr cat ls cat-pred files cat-files noncat-files N)
    (setq ls cat-preds
	  N (length pkg-dir)
	  files (mapcar (lambda (fn) (substring fn N))
			(directory-files-recursively pkg-dir "")))
    (while ls
      (setq pr (pop ls)
	    cat (car pr)
	    cat-pred (cdr pr)
	    noncat-files (make-queue)
	    cat-files (make-queue))
      (mapc (lambda (fn)
	      (if (funcall cat-pred fn)
		  (queue-enqueue cat-files fn)
		(queue-enqueue noncat-files fn)))
	    files)
      (queue-enqueue cq `(,cat . ,(queue-all cat-files)))
      (setq files (queue-all noncat-files)))
    (queue-all cq)))

(defun unboxed--make-package-desc (db pd &optional mgr)
  "Initialize unboxed package descriptor from package-desc PD.
Arguments:
  DB - database containing this package descriptor 
  PD - unboxed package description
  MGR - package installation manager, currently `package' or `unboxed'"
  (unless mgr
    (setq mgr 'package))
  (let ((version (unboxed--get-package-desc-version pd))
	s n)
    (setq s (unboxed-package-desc-create
	     :db db
	     :manager mgr
	     :single (unboxed-package-single-p pd)
	     :simple (unboxed-package-simple-p pd)
	     :version-string version)
	  n (length pd))
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
    (setf (unboxed-package-desc-id s)
	  (unboxed--make-package-desc-id s))
    ;; (setf (unboxed-package-desc-files s)
    ;; 	  (unboxed--catalog-package-files s))
    s))

(defun unboxed--add-package-to-db-packages (db-pkgs pd)
  "Add package descriptor PD to DB-PKGS."
  (unboxed--add-package-name
   (unboxed--db-packages-named db-pkgs)
   pd)
  (unboxed--add-package-id
   (unboxed--db-packages-descs db-pkgs)
   pd))

(defun unboxed--remove-package-from-db-packages (db-pkgs pd)
  "Add package descriptor PD to DB-PKGS."
  (unboxed--remove-package-name
   (unboxed--db-packages-named db-pkgs)
   pd)
  (unboxed--remove-package-id
   (unboxed--db-packages-descs db-pkgs)
   pd))

(defun unboxed--add-package-to-db-state (state pd)
  "Add package descriptor PD to STATE."
  (unboxed--add-package-to-db-packages
   (unboxed--db-state-packages state)
   pd))

(defun unboxed--remove-package-from-db-state (state pd)
  "Add package descriptor PD to STATE."
  (unboxed--remove-package-from-db-packages
   (unboxed--db-state-packages state)
   pd))

(defun unboxed--add-installed-file-to-db-state (state inst)
  "Add installed-file INST to STATE."
  (unboxed--add-installed-file-to-db-files
   (unboxed--db-state-files state)
   inst))

(defun unboxed--remove-installed-file-from-db-state (state inst)
  "Remove installed-file INST from STATE."
  (unboxed--remove-installed-file-from-db-files
   (unboxed--db-state-files state)
   inst))


(defun unboxed--add-package-to-db-packages-delta-remove (delta pd)
  "Add package descriptor PD to DELTA remove."
  (unboxed--add-package-to-db-packages
   (unboxed--db-packages-delta-remove delta)
   pd))

(defun unboxed--add-package-to-db-packages-delta-install (delta pd)
  "Add package descriptor PD to DELTA install."
  (unboxed--add-package-to-db-packages
   (unboxed--db-packages-delta-install delta)
   pd))

(defun unboxed--remove-package-from-db-packages-delta-remove (delta pd)
  "Remove package descriptor PD from DELTA remove."
  (unboxed--remove-package-from-db-packages
   (unboxed--db-packages-delta-remove delta)
   pd))

(defun unboxed--remove-package-from-db-packages-delta-install (delta pd)
  "Remove package descriptor PD from DELTA install."
  (unboxed--remove-package-from-db-packages
   (unboxed--db-packages-delta-install delta)
   pd))


(defun unboxed--add-package-to-db-delta-remove (delta pd)
  "Add package descriptor PD to DELTA remove."
  (unboxed--add-package-to-db-state
   (unboxed--db-delta-remove delta)
   pd))

(defun unboxed--add-package-to-db-delta-install (delta pd)
  "Add package descriptor PD to DELTA install."
  (unboxed--add-package-to-db-state
   (unboxed--db-delta-install delta)
   pd))

(defun unboxed--remove-package-from-db-delta-remove (delta pd)
  "Remove package descriptor PD from DELTA remove."
  (unboxed--remove-package-from-db-state
   (unboxed--db-delta-remove delta)
   pd))

(defun unboxed--remove-package-from-db-delta-install (delta pd)
  "Remove package descriptor PD from DELTA install."
  (unboxed--remove-package-from-db-state
   (unboxed--db-delta-install delta)
   pd))

(defun unboxed--add-installed-file-to-db-delta-remove (delta inst)
  "Add installed-file INST to DELTA remove."
  (unboxed--add-installed-file-to-db-state
   (unboxed--db-delta-remove delta)
   inst))

(defun unboxed--add-installed-file-to-db-delta-install (delta inst)
  "Add installed-file INST to DELTA install."
  (unboxed--add-installed-file-to-db-state
   (unboxed--db-delta-install delta)
   inst))

(defun unboxed--remove-installed-file-from-db-delta-remove (delta inst)
  "Remove installed-file INST from DELTA remove."
  (unboxed--remove-installed-file-from-db-state
   (unboxed--db-delta-remove delta)
   inst))

(defun unboxed--remove-installed-file-from-db-delta-install (delta inst)
  "Remove installed-file INST from DELTA install."
  (unboxed--remove-installed-file-from-db-state
   (unboxed--db-delta-install delta)
   inst))

(defun unboxed--apply-transaction-remove-package (txn pd)
  "Move package descriptor PD from `remove' todo to done of transaction TXN."
  (let ((todo (unboxed--transaction-todo txn))
	(done (unboxed--transaction-done txn)))
    (unboxed--remove-package-from-db-delta-remove todo pd)
    (unboxed--add-package-to-db-delta-remove done pd)
    txn))

(defun unboxed--apply-transaction-install-package (txn pd)
  "Move package descriptor PD from `install' todo to done of transaction TXN."
  (let ((todo (unboxed--transaction-todo txn))
	(done (unboxed--transaction-done txn)))
    (unboxed--remove-package-from-db-delta-install todo pd)
    (unboxed--add-package-to-db-delta-install done pd)
    txn))

(defun unboxed--apply-transaction-remove-file (txn inst)
  "Move installed-file INST from `remove' todo to done of transaction TXN."
  (let ((todo (unboxed--transaction-todo txn))
	(done (unboxed--transaction-done txn)))
    (unboxed--remove-installed-file-from-db-delta-remove todo inst)
    (unboxed--add-installed-file-to-db-delta-remove done inst)
    txn))

(defun unboxed--apply-transaction-install-file (txn inst)
  "Move installed-file INST from `install' todo to done of transaction TXN."
  (let ((todo (unboxed--transaction-todo txn))
	(done (unboxed--transaction-done txn)))
    (unboxed--remove-installed-file-from-db-delta-install todo inst)
    (unboxed--add-installed-file-to-db-delta-install done inst)
    txn))

(defun unboxed--apply-packages-transaction-remove (txn pd)
  "Move package descriptor PD from `remove' todo to done of transaction TXN."
  (let ((todo (unboxed--packages-transaction-todo txn))
	(done (unboxed--packages-transaction-done txn)))
    (unboxed--remove-package-from-db-packages-delta-remove todo pd)
    (unboxed--add-package-to-db-packages-delta-remove done pd)
    txn))

(defun unboxed--apply-packages-transaction-install (txn pd)
  "Move package descriptor PD from `install' todo to done of transaction TXN."
  (let ((todo (unboxed--packages-transaction-todo txn))
	(done (unboxed--packages-transaction-done txn)))
    (unboxed--remove-package-from-db-packages-delta-install todo pd)
    (unboxed--add-package-to-db-packages-delta-install done pd)
    txn))

(defun unboxed--get-package-from-db-packages-by-name (pkgs pkg-name)
  "Get package descriptor for PKG-NAME from db packages PKGS."
  (let ((q (gethash pkg-name
		    (unboxed--db-packages-named pkgs))))
    (when (and q (not (queue-empty q)))
      (queue-first q))))
  

(defun unboxed--get-package-from-state-by-name (state pkg-name)
  "Get package descriptor for PKG-NAME from db state STATE."
  (let ((q (gethash pkg-name
		    (unboxed--db-packages-named
		     (unboxed--db-state-packages state)))))
    (when (and q (not (queue-empty q)))
      (queue-first q))))

(defun unboxed--get-available-package-id (db pkg-id)
  "Get package descriptor for PKG-ID from available packages in DB."
  (let ((q (gethash pkg-id
		    (unboxed--db-packages-descs
		     (unboxed--sexpr-db-available db)))))
    (when (and q (not (queue-empty q)))
      (queue-first q))))

(defun unboxed--get-active-package-id (db pkg-id)
  "Get package descriptor for PKG-ID from active packages in DB."
  (let ((q (gethash pkg-id
		    (unboxed--db-packages-descs
		     (unboxed--db-state-packages
		      (unboxed--sexpr-db-active db))))))
    (when (and q (not (queue-empty q)))
      (queue-first q))))

(defun unboxed--get-cat-from-db-by-name (db catname)
  "Return file-category asociated with CATNAME in database DB."
  (let* ((cats (unboxed--sexpr-db-categories db))
	 (pr (assq catname cats)))
    (and pr (cdr pr))))

(defun unboxed--get-source-file-from-package (pd file-name)
  "Return source-file record for FILE-NAME from unboxed-package-desc PD"
  (gethash file-name (unboxed-package-desc-files pd)))

(defun unboxed--make-boxed-db-state (db packages)
  "Initialize a db state for list of package descriptors PACKAGES."
  (let ((state (unboxed--db-state-create))
	(ls packages)
	pd)
    (while ls
      (setq pd (pop ls))
      (unboxed--add-package-to-db-state
       state
       (unboxed--make-package-desc db pd)))
    state))

(defun unboxed--copy-source-db-files (files)
  "Copy source-file collection FILES."
  (let ((new-files (unboxed--db-files-create)))
    (maphash (lambda (_key aq)
	       (mapc (lambda (src)
		       (unboxed--add-source-file-to-db-files new-files src))
		     (queue-all aq)))
	     (unboxed--db-files-files files))
    new-files))

(defun unboxed--copy-installed-db-files (files)
  "Copy installed-file collection FILES."
  (let ((new-files (unboxed--db-files-create)))
    (maphash (lambda (_key aq)
	       (mapc (lambda (inst)
		       (unboxed--add-installed-file-to-db-files new-files inst))
		     (queue-all aq)))
	     (unboxed--db-files-files files))
    new-files))

(defun unboxed--copy-db-packages (packages)
  "Copy db-packages PACKAGES."
  (let ((new-pkgs (unboxed--db-packages-create)))
    (maphash (lambda (_key aq)
	       (mapc (lambda (pd)
		       (unboxed--add-package-to-db-packages new-pkgs pd))
		     (queue-all aq)))
	     (unboxed--db-packages-descs packages))
    new-pkgs))

(defun unboxed--copy-db-state (state)
  "Copy db-state STATE."
  (let ((new-state
	 (unboxed--db-state-create
	  :packages (unboxed--copy-db-packages
		     (unboxed--db-state-packages state))
	  :files (unboxed--copy-installed-db-files
		     (unboxed--db-state-files state)))))
    new-state))


(defconst unboxed--file-category-customization-type
  `(list (symbol :tag "Name")
	 (symbol :tag "Area")
	 (choice :tag "Path Variable" symbol nil)
	 (choice :tag "Predicate" symbol function)
	 (choice :tag "Location" symbol directory nil)
	 (choice :tag "install-files" symbol function nil)
	 (choice :tag "finalize-install-files" symbol function nil)
	 (choice :tag "remove-files" symbol function nil)
	 (choice :tag "finalize-remove-files" symbol function nil))
  "Customization type for file categories.")

(defconst unboxed--category-list-customization-type
  `(list (repeat :tag "File Categories" ,unboxed--file-category-customization-type))
  "Customization type for list of file categories.")

(defconst unboxed--area-customization-type
  `(list (symbol :tag "Area Name")
	 (choice :tag "Box Directories"
		 (symbol :tag "Variable")
		 (repeat :tag "List"
		  (choice (variable :tag "Variable")
			  (directory :tag "Directory")
			  nil)))
	 (choice :tag "Path to DB"
		 (variable :tag "Variable")
		 (file "Filename"))
	 (choice :tag "Predicate" symbol function nil)
	 (choice :tag "Excluded" symbol nil)
	 (choice :tag "Theme Libs" symbol nil)
	 (choice :tag "Data Directory patterns" symbol nil)
	 (choice :tag "Patches" symbol nil)
	 (choice :tag "Autoloads Filename" symbol string nil)
	 ,unboxed--file-category-customization-type)
  "Customization type for database area.")

(defun unboxed--make-keyword (fld)
  "Create the keyword symbol for FLD."
  (intern (concat ":" (symbol-name fld))))

(defun unboxed--make-struct-layout (name)
  "Create a layout struct recording the NAME struct definition."
  (let ((seq-type (cl-struct-sequence-type name))
	(slot-info (cl-struct-slot-info name))
	keys)
    (setq keys (mapcar (lambda (si) (unboxed--make-keyword (car si))) slot-info))
    (unboxed--struct-layout-create
     :seq-type seq-type
     :keys keys
     :slot-info slot-info)))

(defvar unboxed--struct-layouts
  (mapcar (lambda (name) `(,name . ,(unboxed--make-struct-layout name)))
	  '(unboxed--struct-layout
	    unboxed--area
	    unboxed--sexpr-db
	    unboxed-file-category
	    unboxed-package-desc
	    unboxed-installed-file))
  "Association list of layout descriptors.
Specifies the structs used in unboxed database files.")

(defun unboxed--resolve-conf-list (v)
  "Resolve V to a string or list of strings.
Treats symbols as variables, recurses on lists
until a list of strings is produced."
  (cond
   ((listp v) (mapcan #'unboxed--resolve-conf-list v))
   ((stringp v)   (list v))
   ((boundp v)
    (let ((val (symbol-value v)))
      (if (listp val)
	  (mapcan #'unboxed--resolve-conf-list val)
	(unboxed--resolve-conf-list val))))
   (t nil)))

(defun unboxed--resolve-conf-val (v)
  "Resolve V to a string if it is a symbol."
  (cond
   ((stringp v)   v)
   ((boundp v) (symbol-value v))
   (t nil)))

(defun unboxed--resolve-conf-func (v)
  "Resolve V to a function object if it is a symbol."
  (cond
   ((not (symbolp v))
    (and (functionp v) v))
   ((fboundp v) (symbol-function v))
   ((boundp v) (symbol-value v))
   (t nil)))

(defun unboxed--system-load-path ()
  "Construct the `load-path' used by \"emacs -Q\" process."
  (let ((system-dir (file-name-directory (directory-file-name data-directory)))
	(ls (reverse load-path))
	lisp-dir r)
    (setq lisp-dir (file-name-concat system-dir "lisp"))
    (when ls
      (push (pop ls) r)
      (while (and ls (not (string= (directory-file-name (car r)) lisp-dir)))
	(push (pop ls) r)))
    r))
	

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

(defun unboxed--make-area (name
			   boxes-conf
			   db-path-conf
			   pred-conf
			   excluded-conf
			   theme-libs-conf
			   datadir-conf
			   patches-conf
			   autoloads-conf
			   cats)
  "Create an unboxing area based on configuration values.
NAME
BOXES-CONF
DB-PATH-CONF
PRED-CONF
EXCLUDED-CONF
THEME-LIBS-CONF
DATADIR-CONF
PATCHES-CONF
AUTOLOADS-CONF
CATS"
  (let ((boxes (unboxed--resolve-conf-list boxes-conf))
	(db-path (unboxed--resolve-conf-val db-path-conf))
	(pred (unboxed--resolve-conf-func pred-conf))
	(excluded (unboxed--resolve-conf-val excluded-conf))
	(theme-libs (unboxed--resolve-conf-val theme-libs-conf))
	;(datadir (unboxed--resolve-conf-val datadir-conf))
	(patches (unboxed--resolve-conf-val patches-conf))
	(autoloads-fn (unboxed--resolve-conf-val autoloads-conf)))
    (unboxed--area-create
     :name name
     :boxes boxes
     :db-path db-path
     :pred pred
     :excluded excluded
     :excluded-regex (unboxed--excluded-package-regex excluded)
     :theme-libraries theme-libs
     ;; this should be a variable name
     :datadir-pats datadir-conf
     :patches patches
     :autoloads-file autoloads-fn
     :system-load-path (unboxed--system-load-path)
     :categories cats)))



(define-error 'unboxed-invalid-file-record
  "Unrecognized type of file record")
(define-error 'unboxed-invalid-package
  "Unrecognized package name")
(define-error 'unboxed-invalid-category
  "Unrecognized category name")
(define-error 'unboxed-invalid-category-spec
  "One or more fields in a file category specification is invalid")
(define-error 'unboxed-invalid-install-signature
  "An install method must take 5 arguments")
(define-error 'unboxed-invalid-remove-signature
  "A remove method must take 5 arguments")
(define-error 'unboxed-invalid-finalize-install-signature
  "A finalize-install method must take 5 arguments")
(define-error 'unboxed-invalid-finalize-remove-signature
  "A finalize-remove method must take 5 arguments")

(defun unboxed--import-source-files (pd src-files)
  "Import SRC-FILES record from async process into package descriptor PD"
  (let ((pd-files (unboxed-package-desc-files pd))
	(tbl (unboxed--db-files-files src-files))
	(cats (unboxed--sexpr-db-categories
	       (unboxed-package-desc-db pd))))
    (maphash (lambda (_key src)
	       (let* ((cname (unboxed-file-category-name
			      (unboxed-source-file-db-category src)))
		      (cat (assq cname cats))
		      (fname (symbol-name (unboxed-source-file-file src))))
		 (setq cat (and cat (cdr cat)))
		 (unboxed--add-source-file-to-db-files
		  pd-files
		  (unboxed--make-source-file pd cat fname))))
	     tbl)
    pd))

(defun unboxed--simple-ajq (freq final-k id)
  "Make a simple job queue from FREQ, FINAL-K, and ID."
  (async-job-queue-make-job-queue freq nil final-k nil nil nil id))

(defun unboxed--simple-schedule (ajq program job-id timeout finish-k)
  "Schedule PROGRAM on job queue AJQ with standardized callbacks.
Arguments:
  AJQ - asynchronous job queue
  PROGRAM - sexp of program to run asyncronously
  JOB-ID - identifying symbol for job
  TIMEOUT - timeout in seconds or nil if no timeout
  FINISH-K  - continuation to call when async process returns"
  (ajq-schedule-job ajq
		    program
		    job-id
		    (lambda (_job)
		      (message "Starting %s" job-id))
		    (lambda (_job v)
		      (message "Starting %s: Done" job-id)
		      (funcall finish-k v))
		    timeout
		    (lambda (_job)
		      (message "Starting %s: Timed out" job-id))
		    (lambda (_job)
		      (message "Starting %s: Cancelled" job-id))))

(eval-and-compile
  (defun unboxed--wrap-async-expr (rv prog &optional logfile warnfile msgfile)
    "Wrap sexp PROG for safe running in batch mode, with return value in variable RV."
    (let ((err-sym (cl-gensym "error-")))
      `(progn
	 (defvar ,rv nil)
	 (setq print-circular t)
	 (condition-case ,err-sym
	     (progn
	       (defun yes-or-no-p (prompt)
		 (error "Interactive yes-or-no-p prompting not allowed in batch mode - %S" prompt))
	       (defun y-or-n-p (prompt)
		 (error "Interactive y-or-n-p prompting not allowed in batch mode - %S" prompt))
	       (defun y-or-n-p-with-timeout (prompt seconds default)
		 (error "Interactive y-or-n-p-with-timeout prompting not allowed in batch mode - %S %S %S"
			prompt seconds default))
	       ,prog)
	   (error (display-warning :error (format "%S: %S" (car ,err-sym) (cdr ,err-sym)))))
	 ,@(when logfile
	     `((let ((log-buffer (get-buffer byte-compile-log-buffer)))
		 (when log-buffer
		   (with-current-buffer log-buffer
		     (write-region nil nil ,logfile))))))
	 ,@(when warnfile
	     `((let ((log-buffer (get-buffer "*Warnings*")))
		 (when log-buffer
		   (with-current-buffer log-buffer
		     (write-region nil nil ,warnfile))))))
	 ,@(when msgfile
	     `((let ((log-buffer (get-buffer "*Messages*")))
		 (when log-buffer
		   (with-current-buffer log-buffer
		     (write-region nil nil ,msgfile))))))
	 ,rv))))

(defmacro unboxed--async-expr (rv prog &optional logfile warnfile msgfile)
  "Avoid quoting RV when using `unboxed--wrap-async-expr' on PROG."
  `(unboxed--wrap-async-expr ',rv ,prog ,logfile ,warnfile ,msgfile))


(defun unboxed--check-logfile (logfile)
  "Check if LOGFILE exists and return contents if so, destroying LOGFILE."
  (let (log-text)
    (when (file-exists-p logfile)
      (with-temp-buffer
	(insert-file-contents logfile)
	(setq log-text (buffer-string)))
      (delete-file logfile))
    log-text))
    
(eval-and-compile
  (defun unboxed--format-doc-variable (sym)
    "Format a variable name SYM for appearance in a docstring."
    (let ((s (symbol-name sym)))
      (when (= (aref s 0) ?_)
	(setq s (substring s 1)))
      (upcase s))))


(provide 'unboxed-decls)
;;; unboxed-decls.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ajq-" . "async-job-queue-")("ub-" . "unboxed-")("q-" . "queue-"))
;; End:

