;;; unboxed-decls.el        -*- lexical-binding: t; -*-

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
  categories)

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
   `packages' Assoc list of unboxed-package-descs for unboxed packages in this
              area.
   `installed' Assoc list of unboxed-installed-file structs for each file
              unboxed in the area."
  layouts
  areas
  area
  packages
  installed)

(defun unboxed--sexpr-db-name (db)
  (unboxed--area-name
   (unboxed--sexpr-db-area db)))

(defun unboxed--sexpr-db-boxes (db)
  (unboxed--area-boxes
   (unboxed--sexpr-db-area db)))

(defun unboxed--sexpr-db-path (db)
  (unboxed--area-db-path
   (unboxed--sexpr-db-area db)))

(defun unboxed--sexpr-db-categories (db)
  (unboxed--area-categories
   (unboxed--sexpr-db-area db)))

(defun unboxed--sexpr-db-category-location (db catname)
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
  "Structure for a file that is installed for a package
  Slots:
  `area' name of the unboxing area
  `package' name of package as symbol
  `package-version-string' version of package as a string
  `package-location' directory providing boxed package contents
  `category' category of file as symbol
  `category-location' directory containing installed file
  `package-source' location of source file in directory of package contents
  `file' location of file relative to category-location
         may not be identical to source file, or even have the same
         base name, e.g. byte-compiled files
  `log' Any relevant data generated during the installation process
        for this specific file"
  area
  package
  package-version-string
  package-location
  category
  category-location
  package-source
  file
  log)

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
  `version-string' version string for this package
  `manager' name of installation manager for this package."
  version-string
  manager)

(defconst unboxed--file-category-customization-type
  `(list (symbol :tag "Name")
	 (symbol :tag "Area")
	 (choice :tag "Path Variable" symbol nil)
	 (choice :tag "Predicate" symbol function)
	 (choice :tag "Location" symbol directory nil)
	 (choice :tag "install-files" symbol function nil)
	 (choice :tag "finalize-install-files" symbol function nil)
	 (choice :tag "remove-files" symbol function nil)
	 (choice :tag "finalize-remove-files" symbol function nil)))

(defconst unboxed--sexpr-db-customization-type
  `(list (repeat :tag "File Categories" ,unboxed--file-category-customization-type)))

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
	 ,unboxed--sexpr-db-customization-type))

(defun unboxed--make-keyword (fld)
  (intern (concat ":" (symbol-name fld))))

(defun unboxed--make-struct-layout (name)
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
  "Association list of layout descriptors of the structs used in unboxed database files.")

(defun unboxed--resolve-conf-list (v)
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
  (cond
   ((stringp v)   v)
   ((boundp v) (symbol-value v))
   (t nil)))

(defun unboxed--resolve-conf-func (v)
  (cond
   ((not (symbolp v))
    (and (functionp v) v))
   ((fboundp v) (symbol-function v))
   ((boundp v) (symbol-value v))
   (t nil)))

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
  (let ((boxes (unboxed--resolve-conf-list boxes-conf))
	(db (unboxed--resolve-conf-val db-path-conf))
	(pred (unboxed--resolve-conf-func pred-conf))
	(excluded (unboxed--resolve-conf-val excluded-conf))
	(theme-libs (unboxed--resolve-conf-val theme-libs-conf))
	(datadir (unboxed--resolve-conf-val datadir-conf))
	(patches (unboxed--resolve-conf-val patches-conf))
	(autoloads-fn (unboxed--resolve-conf-val autoloads-conf))
	excluded-regex)
    (unboxed--area-create
     :name name
     :boxes boxes
     :db-path db-path
     :pred pred
     :excluded excluded
     :excluded-regex (unboxed--excluded-package-regex excluded)
     :theme-libraries theme-libs
     :datadir-pats datadir
     :patches patches
     :autoloads-file autoloads-fn
     :categories cats)))



(define-error 'unboxed-invalid-package
  "Unrecognized package name")
(define-error 'unboxed-invalid-category
  "Unrecognized category name")
(define-error 'unboxed-invalid-category-spec
  "One or more fields in a file category specification is invalid")

(defun unboxed--promote-package-desc (pd manager)
  "Takes a packge-desc struct and returns and unboxed-package-desc struct"
  
  )

(provide 'unboxed-decls)
;;; unboxed-decls.el ends here

;; 
