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

(defvar unboxed--buffer-name "*Unboxed*"
  "Name of unboxed logging buffer.")

(defvar unboxed--buffer (get-buffer-create unboxed--buffer-name)
  "The unboxed logging buffer.")


(defun queue-remq (q elt)
  "Remove all occurence of ELT from queue Q"
  (let ((ls (delq elt (queue-all)))
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
  "Remove all occurence of ELT from queue Q"
  (let ((head (queue-all q))
	 ls
	tail0 tail1)
    (setq ls (delete elt head)
	  (setq tail0 ls))
    (when (consp tail0)
      (setq tail1 (cdr tail0)))
    (while tail1
      (setq tail1 (cdr (setq tail0 (cdr tail0)))))
    (setf (queue-head q) ls)
    (setf (queue-tail q) tail0))
  q)

(defun queue-filter (q pred)
  "Remove all occurence of ELT from queue Q"
  (let ((head (queue-all q))
	 ls
	tail0 tail1)
    (setq ls (seq-filter pred head)
	  (setq tail0 ls))
    (when (consp tail0)
      (setq tail1 (cdr tail0)))
    (while tail1
      (setq tail1 (cdr (setq tail0 (cdr tail0)))))
    (setf (queue-head q) ls)
    (setf (queue-tail q) tail0))
  q)

(defun unboxed--get-package-desc-version (pd)
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

(defun unboxed--area-category-location (area catname)
  "Return the location for category CATNAME in AREA."
  (let ((cats (unboxed--area-categories area))
	result)
    (setq result (assq catname cats)
	  result (and result
		      (unboxed-file-category-location (cdr result))))
    result))

(cl-defstruct (unboxed--db-files
	       (:constructor unboxed--db-state-create)
	       (:copier unboxed--db-state-copy))
  "Collection of files associated with packages in db
  Slots:
  `files' - set of unboxed source- or installed- file objects
  `locations' - list of source- or installed- files associated to each category"
  (files (make-hash-table))
  (locations (make-queue)))

(cl-defstruct (unboxed--db-state
	       (:constructor unboxed--db-state-create)
	       (:copier unboxed--db-state-copy))
  "Database state
  Slots:
  `package-descs' set of unboxed package desc object
  `packages' - map package symbols to list of versions in boxed area
  `files' - unboxed--db-files collection"
  (package-descs (make-hash-table))
  (packages (make-hash-table))
  (files (unboxed--db-files-create)))

	      
(cl-defstruct (unboxed--db-delta
	       (:constructor unboxed--db-delta-create)
	       (:copier unboxed--db-delta-copy))
  "The difference between two database states.
  Slots:
  `remove' the db state to eliminate
  `install' the db state requiring installation"
  (remove (unboxed--db-state-create))
  (install (unboxed--db-state-create)))


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

;; note - it's entirely possible for a site to have one version of unboxed installed
;; and for a user to have another version installed.  Therefore, we record
;; the layout of structures in structure itself to allow some forward/backward
;; compatibility
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
   `available' db state for all boxed packages of area on disk
   `active' db state for all boxed packages of area available for package loading"
  layouts
  areas
  area
  (available (unboxed--db-state))
  (active (unboxed--db-state)))

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
  `predicate' predicate to classify file given full path
  `location' path for installing this file category
  `install-files' function to invoke with list of files,
         returns list of unboxed-installed-file structures.
  `finalize-install-files' function to invoke with list of
         unboxed-installed-file structs, run after all packages in a
         set have completed individual installation task.  This
         function performs final steps that should be done once per
         invocation of the installation process on a set of packages,
         e.g. updating the autoloads file in the unboxed package
         directory and byte-compiling the installed libraries with the
         updated autoloads.
  `remove-files' function to invoke when removing packages on
         per-package basis
  `finalize-remove-files' function to invoke when removing packages
         after doing package-specific steps."
  name
  area
  path-variable
  predicate
  location
  install-files
  finalize-install-files
  remove-files
  finalize-remove-files)

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
  `created' boolean indicated whether installing this file succeeded
  `log' Any relevant data generated during the installation process
        for this specific file
  `warnings' *Warnings* buffer during install process
  `messages' *Messages* buffer during install process"
  source
  id
  file
  created
  log
  warnings
  messages)

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
installation manager
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
  (files (unboxed--db-files-create)))

(defun unboxed-package-desc-area (pd)
  (unboxed--sexpr-db-area
   (unboxed-package-desc-db pd)))

(defun unboxed-package-desc-areas (pd)
  (unboxed--sexpr-db-areas
   (unboxed-package-desc-db pd)))

(defun unboxed--make-package-desc-id (pd)
  (let ((name (unboxed-package-desc-name pd))
	(vs (unboxed-package-desc-version-string pd)))
    (intern (concat (symbol-name name) "#" vs))))

(defun unboxed--make-installed-file-id (inst)
  (let ((name (unboxed-installed-file-file inst))
	(vs (unboxed-installed-file-version inst)))
    (intern (concat (symbol-name name) "#" vs))))

(defun unboxed--make-source-file-id (src)
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
	(name (symbol-name (package-desc-name pd)))
	(no-subdirs t)
	all fn)
    (setq all (directory-files d t "^[^.].*$"))
    (while (and no-subdirs all)
      (setq fn (pop all)
	    no-subdirs (not (file-directory-p fn))))
    no-subdirs))

(defun unboxed-package-any-p (pd)
  "Test that succeeds for any package descriptor PD."
  t)

(defun unboxed-package-none-p (pd)
  "Test that fails for any package descriptor PD."
  nil)

(defun unboxed-source-file-version (src)
  (unboxed-package-desc-version-string
   (unboxed-source-file-package-desc src)))

(defun unboxed-source-file-package-location (src)
  (unboxed-package-desc-dir 
   (unboxed-source-file-package-desc src)))

(defun unboxed-source-file-package (src)
  (unboxed-package-desc-name 
   (unboxed-source-file-package-desc src)))
   
(defun unboxed-source-file-category (src)
  (unboxed-file-category-name
   (unboxed-source-file-db-category src)))
   
(defun unboxed-source-file-category-location (src)
  (unboxed-file-category-location
   (unboxed-source-file-db-category src)))

(defun unboxed-installed-file-package-desc (inst)
  (unboxed-source-file-package-desc
   (unboxed-installed-file-source inst)))

(defun unboxed-installed-file-version (inst)
  (unboxed-package-desc-version-string
   (unboxed-installed-file-package-desc inst)))

(defun unboxed-installed-file-package-location (inst)
  (unboxed-package-desc-dir 
   (unboxed-installed-file-package-desc inst)))

(defun unboxed-installed-file-package (inst)
  (unboxed-package-desc-name 
   (unboxed-installed-file-package-desc inst)))
   
(defun unboxed-installed-file-db-category (inst)
  (unboxed-source-file-db-category
   (unboxed-source-file inst)))

(defun unboxed-installed-file-category (inst)
  (unboxed-file-category-name
   (unboxed-installed-file-db-category inst)))
   
(defun unboxed-installed-file-category-location (inst)
  (unboxed-file-category-location
   (unboxed-installed-file-db-category inst)))
   
(defun unboxed--make-category-queue-alist (cats)
  "Make an alist of queues for category set CATS."
  (mapcar (lambda (c-pr)
	    `(,(unboxed-file-category-name (cdr c-pr)) . ,(ajq--make-queue)))
	  cats))

(defun unboxed--exists-source-file-in-cat-queue-p (src aq)
  "Test whether source-file with same category and file as SRC exists in aqueue AQ."
  (let ((cat (unboxed-source-file-category inst))
	(file (unboxed-source-file-file inst))
	pr)
    (setq pr (assq cat (queue-all aq)))
    ;; (unless pr
    ;;   (signal 'unboxed-invalid-category `(,inst ,als)))
    (when pr
      (seq-some (cdr pr)
		(lambda (inst)
		  (eq file
		      (unboxed-source-file-file inst)))))))
      
(defun unboxed--exists-installed-file-in-cat-queue-p (inst aq)
  "Test whether installed-file with same category and file as INST exists in aqueue AQ."
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
      

(defun unboxed--add-source-file-to-cat-queue (src aq)
  "Add src-file SRC to aqueue AQ."
  (let ((cat (unboxed-source-file-category src))
	pr)
    (setq pr (assq cat (queue-all aq)))
    ;; (unless pr
    ;;   (signal 'unboxed-invalid-category `(,inst ,als)))
    (unless pr
      (setq pr (cons cat (make-queue)))
      (queue-enqueue aq pr))
    (queue-enqueue (cdr pr) inst))
  aq)

;; for use in a package-specific set of source files
(defun unboxed--add-source-file (src sources)
  "Add source file SRC to hash table SOURCES by its package relative path."
  (let ((k (unboxed--source-file-file src))
	q)
    (setq q (or (gethash k sources)
		(puthash k (make-queue) sources)))
    (queue-enqueue q src))
  sources)

;; for use in db-specific set of source files
(defun unboxed--add-source-file-id (src sources)
  "Add source file SRC to hash table SOURCES by its identity"
  (let ((k (unboxed--source-file-file src))
	q)
    (setq q (or (gethash k sources)
		(puthash k (make-queue) sources)))
    (queue-enqueue q src))
  sources)

(defun unboxed--add-installed-file-to-cat-queue (inst aq)
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
  "Return queue asociated with CATNAME in association-queue AQ"
  (let ((q (assq cat (queue-all aq))))
    (when q
      (setq q (cdr q)))
    q))

(defun unboxed--add-installed-file (inst installed)
  "Add installed file INST to hash table INSTALLED by its standard id."
  (let ((k (unboxed--installed-file-id inst))
	q)
    (setq q (or (gethash k installed)
		(puthash k (make-queue) installed)))
    (queue-enqueue q inst))
  installed)

(defun unboxed--add-installed-file-source (inst sources)
  "Add installed file INST to hash table SOURCES by its source path"
  (let ((k (unboxed--installed-file-source inst))
	q)
    (setq q (or (gethash k sources)
		(puthash k (make-queue) sources)))
    (queue-enqueue q inst))
  sources)

(defun unboxed--add-installed-file-to-files (files inst)
  "Add INST to FILES."
  (unboxed--add-installed-file-to-cat-queue 
   inst
   (unboxed--db-files-locations state))
  (unboxed--add-installed-file
   inst
   (unboxed--transaction-state-files state)))

(defun unboxed--add-source-file-to-files (files src)
  "Add SRC to FILES."
  (unboxed--add-source-file-to-cat-queue 
   inst
   (unboxed--db-files-locations files))
  (unboxed--add-source-file
   src
   (unboxed--db-files-files files)))

(defun unboxed--add-package-name (pd packages)
  "Add package descriptor PD to hash table PACKAGES by its name."
  (let ((k (unboxed-package-desc-name pd))
	q)
    (setq q (or (gethash k packages)
		(puthash k (make-queue) packages)))
    (queue-enqueue q pd))
  packages)

(defun unboxed--add-package-id (pd packages)
  "Add package descriptor PD to hash table PACKAGES by its name."
  (let ((k (unboxed-package-desc-id pd))
	q)
    (setq q (or (gethash k packages)
		(puthash k (make-queue) packages)))
    (queue-enqueue q pd))
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

(defun unboxed--make-installed-file (src dst-file)
  "Make installed-file structure.
Arguments:
  SRC - source-file structure
  DST-FILE - file relative to unboxing category location"
  (let ((inst
	 (unboxed-installed-file-create :source src
					:file dst-file)))
    (setf (unboxed-installed-file-id inst)
	  (unboxed--make-installed-file-key inst))
    inst))


(defun unboxed--catalog-package-files (pd)
  "Construct source files for package PD."
  (message "Cataloging files for %s" pd)
  (let ((db (unboxed-package-desc-db pd))
	(pd-files (unboxed-package-desc-files pd))
	(pkg-dir (file-name-as-directory (unboxed-package-desc-dir pd)))
	area pkgs installed
	cats ls install-files cat-pred cat-files noncat-files N
	cat-name cat-installed-files-pair
	cat-installed-files all-installed pr)
    (setq area (unboxed--sexpr-db-area db)
	  pkgs (unboxed--sexpr-db-packages db)
	  installed (unboxed--sexpr-db-installed db)
	  cats (unboxed--area-categories area)
	  ls cats
	  N (length pkg-dir)
	  files (mapcar (lambda (fn) (substring fn N))
			(directory-files-recursively pkg-dir "")))
    (while ls
      (setq cat (cdr (pop ls))
	    cat-files (make-queue)
	    noncat-files (make-queue)
	    cat-pred (unboxed-file-category-predicate cat))
      (mapc (lambda (fn)
	      (if (funcall cat-pred fn)
		  (unboxed--add-source-file-to-files
		   pd-files
		   (make-source-file pd cat fn))
		(queue-enqueue noncat-files fn)))
	    files))
    pd-files))

(defun unboxed--make-package-desc (pd &optional mgr)
  "Initialize unboxed package descriptor from package-desc PD.
Arguments:
  PD - unboxed package description
  MGR - package installation manager, `package' or `unboxed'"
  (unless mgr
    (setq mgr 'package))
  (let ((version (unboxed--get-package-desc-version pd))
	s n)
    (setq s (unboxed-package-desc-create
	     :manager 'package
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
    (setf (unboxed-package-desc-files s)
	  (unboxed--catalog-package-files s))
    s))

(defun unboxed--add-package-to-db-state (state pd)
  "Add package descriptor PD to STATE."
  (unboxed--add-package-name
   pd
   (unboxed--db-state-packages state))
  (unboxed--add-package-id
   inst
   (unboxed--db-state-package-descs state)))

(defun unboxed--make-boxed-db-state (packages)
  "Initialize a db state for list of package descriptors PACKAGES."
  (let ((state (unboxed--db-state-create))
	(ls packages)
	pd)
    (while ls
      (setq pd (pop ls))
      (unboxed--add-package-to-db-state
       state
       (unboxed--make-package-desc pd)))
    state))

(defun unboxed--copy-source-db-files (files)
  "Copy source-file collection FILES."
  (let ((new-files (unboxed--db-files-create)))
    (maphash (lambda (key aq)
	       (mapc (lambda (src)
		       (unboxed--add-source-file-to-files new-files src))
		     (queue-all aq)))
	     (unboxed--db-files-files files))
    new-files))

(defun unboxed--copy-installed-db-files (files)
  "Copy installed-file collection FILES."
  (let ((new-files (unboxed--db-files-create)))
    (maphash (lambda (key aq)
	       (mapc (lambda (inst)
		       (unboxed--add-installed-file-to-files new-files inst))
		     (queue-all aq)))
	     (unboxed--db-files-files files))
    new-files))

(defun unboxed--copy-db-state (state)
  "Copy db-state STATE."
  (let ((new-state (unboxed--db-state-create)))
    (maphash (lambda (key aq)
	       (mapc (lambda (pd)
		       (unboxed--add-package-desc-to-state new-state pd))
		     (queue-all aq)))
	     (unboxed--db-state-package-descs files))
    new-files))


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
	 ,unboxed--sexpr-db-customization-type)
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



(define-error 'unboxed-invalid-package
  "Unrecognized package name")
(define-error 'unboxed-invalid-category
  "Unrecognized category name")
(define-error 'unboxed-invalid-category-spec
  "One or more fields in a file category specification is invalid")


(provide 'unboxed-decls)
;;; unboxed-decls.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ajq-" . "async-job-queue-")("ub-" . "unboxed-")("q-" . "queue-"))
;; End:

