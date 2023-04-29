;;; unboxed-categories.el --- File category implemenation for unboxed     -*- lexical-binding: t; -*-

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

;; Functions for implementing file-category definitions

;;; Code:

(require 'unboxed-decls)

(defvar unboxed-theme-libraries
  '(apropospriate-theme)
  "Libraries for themes that end in -theme but are not theme files.")

;;; libraries to install in the package lisp directory
(defun unboxed-library-p (path)
  "Test whether PATH is an elisp library source file on the `load-path'.
This predicate only recognizes files matching `*.el' in top directory
of the package archive, and excludes `*-pkg.el' and `*-autoloads.el'
files, since the former are not proper elisp and the latter are not
useful in an unboxed installation."
  (let ((ext (file-name-extension path))
	(base (file-name-sans-extension (file-name-nondirectory path)))
	(dir (file-name-directory path)))
    (and ext (string= ext "el")
	 (not dir)
	 (or (not (string-suffix-p "-theme" base))
	     (memq (intern base) unboxed-theme-libraries))
	 (not (string-suffix-p "-pkg" base))
	 (not (string-suffix-p "-autoloads" base)))))

(defun unboxed-data-library-p (path)
  "Test if PATH is an elisp library source file not on the load path.
These will be installed in the unboxed data directory for the package and
compiled there."
  (let ((ext (file-name-extension path))
	(base (file-name-sans-extension (file-name-nondirectory path))))
    (and ext (string= ext "el")
	 (not (string-suffix-p "-pkg" base))
	 (not (string-suffix-p "-autoloads" base)))))

;;; modules to install in the package lisp directory
(defun unboxed-module-p (path)
  "Test if PATH is an elisp module (dynamic library).
Modules will be installed in the unboxed library directory."
  (let ((ext (file-name-extension path)))
    (and ext (string= ext "so"))))

;;; theme files
(defun unboxed-theme-p (path)
  "Test if PATH is a theme file to be installed in the unboxed theme directory.
Any `*-theme.el' file whose feature name is not contained in
`unboxed-theme-libraries' variable is classified as a theme."
  (let ((ext (file-name-extension path))
	(base (file-name-sans-extension (file-name-nondirectory path))))
    (and ext (string= ext "el")
	 (string-suffix-p "-theme" base)
	 (not (memq (intern base) unboxed-theme-libraries)))))

;;; info files
(defun unboxed-info-p (path)
  "Test if PATH is an info file.
This predicate recognizes all `*.info' and 'dir' files as info files.
The `.info' files will be installed in the unboxed info directory."
  (let ((ext (file-name-extension path)))
    (or (and ext (string= ext "info"))
	(string= (file-name-nondirectory path) "dir"))))

;;; files to ignore
(defun unboxed-compiled-elisp-p (path)
  "Test if PATH is a compiled library of some form."
  (let ((ext (file-name-extension path)))
    (and ext
	 (or (string= ext "elc")
	     (string= ext "eln")))))

(defun unboxed-byte-compiled-p (path)
  "Test if PATH is a byte-compiled library."
  (let ((ext (file-name-extension path)))
    (and ext (string= ext "elc"))))

(defun unboxed-native-compiled-p (path)
  "Test if PATH is a native-compiled library."
  (let ((ext (file-name-extension path)))
    (and ext (string= ext "eln"))))

;;; Anything else
(defun unboxed-data-p (_path)
  "Test if _PATH should be copied to unboxed package's data directory.
This predicate is expected to be executed last and capture all files
not explicitly ignored."
  t)

(defun unboxed--function-or-nil (val)
  "Test if VAL is a function symbol or any non-symbol value."
  (cond
   ((symbolp val)
    (and (fboundp val) (symbol-function val)))
   (t val)))

(defun unboxed--string-or-nil (val)
  "Test if VAL is a variable or any non-symbol value."
  (cond
   ((symbolp val)
    (and (boundp val) (symbol-value val)))
   (t val)))

(cl-defgeneric unboxed-category-predicate (category area location file)
  "Test whether FILE  in LOCATION is in CATEGORY for unboxing AREA.
Arguments:
  CATEGORY
  AREA
  LOCATION
  FILE"
  nil)

(cl-defgeneric unboxed-install-package-category (category area pkg pkg-box srcs data-box)
  "Install method for files of an unboxed package belonging to category.
Arguments:
  CATEGORY
  AREA
  PKG
  PKG-BOX
  SRCS
  DATA-BOX"
  nil)

  
(cl-defgeneric unboxed-initialize-install-category-files (category area files)
  "Initialize install method for installed files belonging to category.
Arguments:
  CATEGORY - symbol identifying the category or unboxed-file-category
  AREA - unboxing area record
  FILES - installed files of category"
  nil)

(cl-defgeneric unboxed-finalize-install-category-files (category area files)
  "Finalize install method for installed files belonging to category.
Arguments:
  CATEGORY - symbol identifying the category or unboxed-file-category
  AREA - unboxing area record
  FILES - installed files of category"
  nil)


(cl-defgeneric unboxed-remove-package-category (category area pkg pkg-box files)
  "Remove method for files of an unboxed package belonging to category.
Arguments:
  CATEGORY
  AREA
  PKG
  PKG-BOX
  FILES"
  nil)

(cl-defgeneric unboxed-initialize-remove-category-files (category area files)
  "Initialize remove method for installed files belonging to category.
Arguments:
  CATEGORY
  AREA
  FILES"
  nil)

(cl-defgeneric unboxed-finalize-remove-category-files (category area files)
  "Finalize remove method for installed files belonging to category.
Arguments:
  CATEGORY
  AREA
  FILES"
  nil)

(defmacro unboxed--define-file-category-helper (name area params &rest clauses)
  "Helper macro for `unboxed-define-file-category'.
Arguments:
   NAME - category name
   AREA - area name
   PARAMS - accumulated parameters for category structure
   CLAUSES - remaining clauses to be processed"
  (if (null clauses)
      `(unboxed--add-file-category-to-area
	',area
	(unboxed-file-category-create :name ',name :area ',area ,@params))
    (let ((area-val (or (unboxed--lookup-area area)
			(error "Undefined unboxing area %s" area))))
      (pcase clauses
	(`((path ,path). ,remaining)
	 `(unboxed--define-file-category-helper ,name ,area
						(,@params :path-var ',path)
						,@remaining))
	(`((location ,location) . ,remaining)
	 `(unboxed--define-file-category-helper ,name ,area
						(,@params :location ',location)
						,@remaining))
	(`((libraries . ,libraries) . ,remaining)
	 `(unboxed--define-file-category-helper ,name ,area
						(,@params :libraries ',libraries)
						,@remaining))
	(`((predicate . ,body) . ,remaining)
	 `(progn
	    (cl-defmethod unboxed-category-predicate
	      ((category (eql ',name))
	       (area (eql ',area))
	       location file)
	      ,(format "Test file for membership in category %s in area %s." name area)
	      (unboxed-category-predicate ',name ,area-val location file))
	    (cl-defmethod unboxed-category-predicate
	      ((category (eql ',name))
	       (area (eql ,area-val))
	       location file)
	      ,(format "Test file for membership in category %s in area %s." name area)
	      ,@body)
	    (unboxed--define-file-category-helper ,name ,area ,params ,@remaining)))
	(`((install . ,body) . ,remaining)
	 `(progn
	    (cl-defmethod unboxed-install-package-category
	      ((category (eql ',name))
	       (area (eql ',area))
	       pkg pkg-box srcs data-box)
	      ,(format "Install files in category %s of package in area %s." name area)
	      (unboxed-install-package-category ',name ,area-val pkg pkg-box srcs data-box))
	    (cl-defmethod unboxed-install-package-category
	      ((category (eql ',name))
	       (area (eql ,area-val))
	       pkg pkg-box srcs data-box)
	      ,(format "Install files in category %s of package in area %s." name area)
	      ,@body)
	    (unboxed--define-file-category-helper ,name ,area ,params ,@remaining)))
	(`((remove . ,body) . ,remaining)
	 `(progn
	    (cl-defmethod unboxed-remove-package-category
	      ((category (eql ',name))
	       (area (eql ',area))
	       pkg pkg-box files)
	      ,(format "Remove files in category %s of package in area %s." name area)
	      (unboxed-remove-package-category ',name ,area-val pkg pkg-box files))
	    (cl-defmethod unboxed-remove-package-category
	      ((category (eql ',name))
	       (area (eql ,area-val))
	       pkg pkg-box files)
	      ,(format "Remove files in category %s of package in area %s." name area)
	      ,@body)
	    (unboxed--define-file-category-helper ,name ,area ,params ,@remaining)))
	(`((initialize-install . ,body) . ,remaining)
	 `(progn
	    (cl-defmethod unboxed-initialize-install-category-files
	      ((category (eql ',name))
	       (area (eql ',area))
	       files)
	      ,(format
		(concat "Initialize install of installed files"
			"in category %s of package in area %s.")
		name area)
	      (unboxed-initialize-install-category-files ',name ,area-val files))
	    (cl-defmethod unboxed-initialize-install-category-files
	      ((category (eql ',name))
	       (area (eql ,area-val))
	       files)
	      ,(format
		(concat "Initialize install of installed files"
			"in category %s of package in area %s.")
		name area)
	      ,@body)
	    (unboxed--define-file-category-helper ,name ,area ,params ,@remaining)))
	(`((finalize-install . ,body) . ,remaining)
	 `(progn
	    (cl-defmethod unboxed-finalize-install-category-files
	      ((category (eql ',name))
	       (area (eql ',area))
	       files)
	      ,(format
		(concat "Finalize install of installed files"
			"in category %s of package in area %s.")
		name area)
	      (unboxed-finalize-install-category-files ',name ,area-val files))
	    (cl-defmethod unboxed-finalize-install-category-files
	      ((category (eql ',name))
	       (area (eql ,area-val))
	       files)
	      ,(format
		(concat "Finalize install of installed files"
			"in category %s of package in area %s.")
		name area)
	      ,@body)
	    (unboxed--define-file-category-helper ,name ,area ,params ,@remaining)))
	(`((initialize-remove . ,body) . ,remaining)
	 `(progn
	    (cl-defmethod unboxed-initialize-remove-category-files
	      ((category (eql ',name))
	       (area (eql ',area))
	       files)
	      ,(format
		(concat "Initialize remove of installed files"
			"in category %s of package in area %s.")
		name area)
	      (unboxed-initialize-remove-category-files ',name ,area-val files))
	    (cl-defmethod unboxed-initialize-remove-category-files
	      ((category (eql ',name))
	       (area (eql ,area-val))
	       files)
	      ,(format
		(concat "Initialize remove of installed files"
			"in category %s of package in area %s.")
		name area)
	      ,@body)
	    (unboxed--define-file-category-helper ,name ,area ,params
						  ,@remaining)))
	(`((finalize-remove . ,body) . ,remaining)
	 `(progn
	    (cl-defmethod unboxed-finalize-remove-category-files
	      ((category (eql ',name))
	       (area (eql ',area))
	       files)
	      ,(format
		(concat "Finalize remove of installed files"
			"in category %s of package in area %s.")
		name area)
	      (unboxed-finalize-remove-category-files ',name ,area-val files))
	    (cl-defmethod unboxed-finalize-remove-category-files
	      ((category (eql ',name))
	       (area (eql ,area-val))
	       files)
	      ,(format
		(concat "Finalize remove of installed files"
			"in category %s of package in area %s.")
		name area)
	      ,@body)
	    (unboxed--define-file-category-helper ,name ,area ,params
						  ,@remaining)))
	(unrecognized
	 (error "Unrecognized clause unboxed-define-file-category %s %s %S"
		name area unrecognized))))))

(defmacro unboxed-define-file-category (name area &rest clauses)
  "Define a file-category NAME in unboxing area AREA according to CLAUSES.
Clauses may have the form:
  (path PATH-VARIABLE) - the path variable associated with this category
  (location LOCATION) - the directory in which category files will be
                        installed
  (libraries LIBRARY0 LIBRARY1 ...) -
                   list of libraries that must be loaded for the generic method
                   definitions of this category.
                   Unboxed libraries are implicitly added.
  (predicate BODY ...) - define `unboxed-category-predicate' method
                         Arguments (category area location file)
                             bound in BODY...
  (install BODY ...) - define `unboxed-install-package-category' method
                       Arguments (category area pkg pkg-box srcs data-box)
                           bound in BODY...
  (remove BODY ...) - define a `unboxed-remove-package-category' method
                       Arguments (category area pkg pkg-box files)
                           bound in BODY...
  (initialize-install BODY ...) -
                  define `unboxed-finalize-install-category-files' method
                  Arguments (category area files)
                       bound in BODY...
  (finalize-install BODY ...) -
                  define `unboxed-finalize-install-category-files' method
                  Arguments (category area files)
                       bound in BODY...
  (initialize-remove BODY ...) -
                  define `unboxed-finalize-remove-category-files' method
                  Arguments (category area files)
                       bound in BODY...
  (finalize-remove BODY ...) -
                  define `unboxed-finalize-remove-category-files' method
                  Arguments (category area files)
                       bound in BODY..."
  (if (and (symbolp name)
	   (symbolp area))
      `(unboxed--define-file-category-helper ,name ,area () ,@clauses)
    (error "unboxed-define-file-category requires name and area symbols, got %S %S" name area)))

(defmacro unboxed-define-simple-category (name area location &optional
					       pred path-var
					       install remove
					       initialize-install initialize-remove
					       finalize-install finalize-remove)
  "Define a library category in AREA.
Arguments:
  NAME - name of category
  AREA - name of unboxing area
  LOCATION - variable or string specifying the location of unboxed libraries
  PRED - predicate function to call with file path
  PATH-VAR - variable or string specifing the load-path for files of this type
  INSTALL - function symbol to call for installing package files
  REMOVE -  function symbol to call for removing package files
  INITIALIZE-INSTALL - function symbol to call for finalizing installed files
  INITIALIZE-REMOVE -  function symbol to call for finalizing removed files
  FINALIZE-INSTALL - function symbol to call for finalizing installed files
  FINALIZE-REMOVE -  function symbol to call for finalizing removed files"
  `(unboxed-define-file-category
    ,name ,area
    (location ,location)
    ,@(unless (null pred)
	`((predicate
	   (,pred (file-name-concat location file)))))
    ,@(unless (null path-var) `((path ,path-var)))
    ,@(unless (null install)
	`((install
	   (,install category area pkg pkg-box srcs data-box))))
    ,@(unless (null remove)
	`((remove
	   (,remove category area pkg pkg-box files))))
    ,@(unless (null initialize-install)
	`((initialize-install
	   (,initialize-install category area files))))
    ,@(unless (null initialize-remove)
	`((initialize-remove
	   (,initialize-install category area files))))
    ,@(unless (null finalize-install)
	`((finalize-install
	   (,finalize-install category area files))))
    ,@(unless (null finalize-remove)
       `((finalize-remove
	 (,finalize-install category area files))))))

(defmacro unboxed-define-theme-category (area location)
  "Define a theme category in AREA.
Arguments:
  AREA - name of unboxing area
  LOCATION - variable or string specifying the location of unboxed theme files"
  `(unboxed-define-simple-category theme
				   ,area
				   ,location
				   unboxed-theme-p
				   custom-theme-load-path
				   unboxed--basic-category-files-install
				   unboxed--basic-category-files-remove))

(defmacro unboxed-define-library-category (area location)
  "Define a library category in AREA.
Arguments:
  AREA - name of unboxing area
  LOCATION - variable or string specifying the location of unboxed libraries"
  `(unboxed-define-simple-category library
				   ,area
				   ,location
				   unboxed-library-p
				   load-path
				   unboxed--library-category-files-install
				   unboxed--basic-category-files-remove
				   unboxed--update-autoloads-file
				   unboxed--basic-files-remove
				   unboxed--byte-compile-installed-libraries
				   unboxed--update-autoloads-file))

(defmacro unboxed-define-byte-compiled-category (area location)
  "Define a byte-compiled category in AREA.
Arguments:
  AREA - name of unboxing area
  LOCATION - variable or string specifying the location of unboxed byte-compiled files"
  `(unboxed-define-simple-category byte-compiled
				   ,area
				   ,location
				   nil
				   load-path
				   nil
				   unboxed--basic-category-files-remove
				   nil
				   nil
				   nil
				   unboxed--basic-files-remove))

(defmacro unboxed-define-native-compiled-category (area location)
  "Define a native-compiled category in AREA.
Arguments:
  AREA - name of unboxing area
  LOCATION - variable or string specifying the location of unboxed native-compiled files"
  `(unboxed-define-simple-category byte-compiled
				   ,area
				   ,location
				   nil
				   native-comp-eln-load-path
				   nil
				   unboxed--basic-category-files-remove
				   nil
				   nil
				   nil
				   unboxed--basic-files-remove))

(defmacro unboxed-define-module-category (area location)
  "Define a module category in AREA.
Arguments:
  AREA - name of unboxing area
  LOCATION - variable or string specifying the location of
             unboxed module (shared library) files"
  `(unboxed-define-simple-category module
				   ,area
				   ,location
				   unboxed-module-p
				   load-path
				   unboxed--basic-category-files-install
				   unboxed--basic-category-files-remove))

(defmacro unboxed-define-info-category (area location)
  "Define an info category in AREA.
Arguments:
  AREA - name of unboxing area
  LOCATION - variable or string specifying the location of
             unboxed info files"
  `(unboxed-define-simple-category info
				   ,area
				   ,location
				   unboxed-info-p
				   Info-directory-list
				   unboxed--basic-category-files-install
				   unboxed--info-category-files-remove
				   nil
				   nil
				   unboxed--info-files-finalize-install))

(defmacro unboxed-define-ignore-category (area)
  "Define an ignored category in AREA.
Arguments:
  AREA - name of unboxing area"
  `(unboxed-define-simple-category info
				   ,area
				   nil
				   unboxed-compiled-elisp-p))

(defmacro unboxed-define-data-library-category (area location)
  "Define a data-library category in AREA.
Arguments:
  AREA - name of unboxing area
  LOCATION - variable or string specifying the location of unboxed data libraries"
  `(unboxed-define-simple-category data-library
				   ,area
				   ,location
				   unboxed-data-library-p
				   nil
				   nil
				   unboxed--basic-category-files-remove
				   nil
				   unboxed--basic-files-remove
				   unboxed--byte-compile-data-libraries
				   nil))

(defmacro unboxed-define-data-category (area location)
  "Define a data category in AREA.
Arguments:
  AREA - name of unboxing area
  LOCATION - variable or string specifying the location of unboxed data files"
  `(unboxed-define-simple-category data
				   ,area
				   ,location
				   unboxed-data-p
				   nil
				   unboxed--relative-category-files-install
				   unboxed--basic-category-files-remove))

(unboxed-define-theme-category user unboxed-user-theme-directory)
(unboxed-define-theme-category site unboxed-site-theme-directory)

(unboxed-define-library-category user unboxed-user-library-directory)
(unboxed-define-library-category site unboxed-site-library-directory) 

(unboxed-define-byte-compiled-category user unboxed-user-library-directory)
(unboxed-define-byte-compiled-category site unboxed-site-library-directory)

(unboxed-define-native-compiled-category user unboxed-user-native-compiled-directory)
(unboxed-define-native-compiled-category site unboxed-site-native-compiled-directory)

(unboxed-define-module-category user unboxed-user-library-directory)
(unboxed-define-module-category site unboxed-site-library-directory)

(unboxed-define-info-category user unboxed-user-info-directory)
(unboxed-define-info-category site unboxed-site-info-directory)

(unboxed-define-ignore-category user)
(unboxed-define-ignore-category site)

(unboxed-define-data-library-category user unboxed-user-data-directory)
(unboxed-define-data-library-category site unboxed-site-data-directory)

(unboxed-define-data-category user unboxed-user-data-directory)
(unboxed-define-data-category site unboxed-site-data-directory)





(provide 'unboxed-categories)

;;; unboxed-categories.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ajq-" . "async-job-queue-")("ub-" . "unboxed-")("q-" . "queue-"))
;; End:
;; 
