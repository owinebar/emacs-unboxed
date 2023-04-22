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


(cl-defstruct (unboxed--transaction-state
	       (:constructor unboxed--transaction-state-create)
	       (:copier unboxed--transaction-state-copy))
  "Database state
  Slots:
  `package-descs' set of unboxed package desc object
  `packages' - map package symbols to list of versions in boxed area
  `
  `files' - set of unboxed installed file objects
  `locations' - list of installed files in each category location"
  packages
  files
  locations )

(defun ub--make-transaction-state (packages)
  "Initialize a transaction state for list of PACKAGES."
  (unb--transaction-state-create
   :packages (
	      
(cl-defstruct (unboxed--transaction-delta
	       (:constructor unboxed--transaction-delta-create)
	       (:copier unboxed--transaction-delta-copy))
  "The difference between two database states.
  Slots:
  `remove' the db state to eliminate
  `install' the db state requiring installation"
  remove
  install)


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
  initial
  todo
  done
  final
  on-completion)

;; note - it's entirely possible for a site to have one version of unboxed installed
;; and for a user to have another version installed.  Therefore, we record
;; the layout of structures in structure itself to allow some forward/backward
;; compatibility
(cl-defstruct (unboxed--sexpr-db
	       (:constructor unboxed--sexpr-db-create)
	       (:copier unboxed--sexpr-db-copy))
  "Structure holding the tables of data for unboxed in sexpr db representation
   Slots:
   `layouts' Association list of data structure layouts used in this db
   `areas' Association list of area structs in scope for dependency calculations
   `area' area struct for this database
   `packages' Hash table of unboxed-package-descs for unboxed packages in this
              area
   `files' Hash table of unboxed-installed-file structs for each file
              unboxed in the area
   `locations' list of files in each file category location"
  layouts
  areas
  area
  packages
  installed
  locations)

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

(cl-defstruct (unboxed-installed-file
               (:constructor unboxed-installed-file-create)
	       (:copier unboxed-installed-file-struct-copy))
  "Structure for a file that is installed for a package.
An installed-file record may be created even if the installation of
the file failed, so that the messages/warnings/log will be kept
for reference.
  Slots:
  `area' name of the unboxing area
  `package' name of package as symbol
  `version' version of package as a string
  `package-location' directory providing boxed package contents
  `category' category of file as symbol
  `category-location' directory containing installed file
  `package-source' location of source file in directory of package contents
  `file' location of file relative to category-location
         may not be identical to source file, or even have the same
         base name, e.g. byte-compiled files
  `created' boolean indicated whether installing this file succeeded
  `log' Any relevant data generated during the installation process
        for this specific file
  `warnings' *Warnings* buffer during install process
  `messages' *Messages* buffer during install process"
  area
  package
  version
  package-location
  category
  category-location
  package-source
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
  `single' boolean which is t if the package is for a single library file
  `simple' boolean which is t if the package directory has no subdirectories
  `version-string' version string for this package
  `manager' name of installation manager for this package."
  single
  simple
  (version-string "0")
  (manager 'package))

(defun unboxed--package-desc-key (pd)
  (let ((name (unboxed-package-desc-name pd))
	(vs (unboxed-package-desc-version-string pd)))
    (intern (concat (symbol-name name) "@" vs)))) 

(defun unboxed--installed-file-key (inst)
  (let ((name (unboxed-installed-file-file inst))
	(vs (unboxed-installed-file-version inst)))
    (intern (concat (symbol-name name) "@" vs))))

(defun unboxed--installed-file-source-key (inst)
  (let ((name (unboxed-installed-file-name inst))
	(vs (unboxed-installed-file-version inst)))
    (intern (concat (symbol-name name) "@" vs))))

(defun unboxed-package-single-p (pd)
  "Test whether package descriptor PD is for a single file package."
  (let ((d (package-desc-dir pd))
	(name (symbol-name (package-desc-name pd)))
	all main auto pkg r)
    (when (and d (file-accessible-directory-p d))
      (setq all (directory-files d nil "^[^.].*$")
	    main (directory-files d nil (concat "^" (regexp-quote name) "\\.elc?$"))
	    auto (directory-files d nil (concat "^" (regexp-quote name) "-autoloads\\.elc?$"))
	    pkg (directory-files d nil (concat "^" (regexp-quote name) "-pkg\\.elc?$")))
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

