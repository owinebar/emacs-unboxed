;;; unboxed.el --- Unboxed package management        -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Onnie Winebarger

;; Author: Onnie Winebarger;; Copyright (C) 2023 by Onnie Lynn Winebarger <owinebar@rapscallion>
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

;; Unboxed installs packages in a single "packages" directory whenever possible
;; 

;;; Code:

(require 'package)
(require 'cl-lib)
(require 'seq)

(defgroup unboxed nil
  "Manager for unboxed Emacs Lisp packages."
  :group 'package
  :version "24.1")

(defgroup unboxed-system nil
  "Manager for unboxed Emacs Lisp packages installed at system-level."
  :group 'unboxed
  :version "24.1")

(defcustom unboxed-user-db-path user-emacs-directory
  "Directory in which the database tracking installed user packages
will be stored" 
  :type 'directory
  :group 'unboxed)
(defcustom unboxed-user-db-file "packages.sqlite"
  "File name of database tracking installed user packages"
  :type 'filename
  :group 'unboxed)
(defcustom unboxed-system-db-path data-directory
  "Directory in which the database tracking installed system packages
will be stored" 
  :type 'directory
  :group 'unboxed-system)
(defcustom unboxed-system-db-file "system-packages.sqlite"
  "File name of database tracking installed system packages"
  :type 'filename
  :group 'unboxed-system)

(defcustom unboxed-user-package-archive package-user-dir
  "Directory in which unpacked user packages are found"
  :type 'directory
  :group 'unboxed)
(defcustom unboxed-user-library-directory
  (file-name-concat user-emacs-directory "lisp")
  "Directory in which unboxed elisp libraries from user packages will
be installed.  Will be added to the load-path." 
  :type 'directory
  :group 'unboxed)
(defcustom unboxed-user-themes-directory
  (file-name-concat user-emacs-directory "themes")
  "Directory in which unboxed theme files from user packages will be
installed." 
  :type 'directory
  :group 'unboxed)
(defcustom unboxed-user-info-directory
  (file-name-concat user-emacs-directory "info")
  "Directory in which unboxed info files from user packages will be
installed." 
  :type 'directory
  :group 'unboxed)
(defcustom unboxed-user-data-directory
  (file-name-concat user-emacs-directory "data")
  "The directory in which package data directories for unboxed user
packages will be installed." 
  :type 'directory
  :group 'unboxed)

(defcustom unboxed-system-directory
  (expand-file-name (file-name-concat data-directory ".."))
  "The version-specific directory in which emacs is installed"
  :type 'directory
  :group 'unboxed-system)
(defcustom unboxed-system-unversioned-directory
  (if (string= (file-name-nondirectory unboxed-system-directory)
	       (format "%s.%s" emacs-major-version emacs-minor-version))
      (expand-file-name (file-name-concat unboxed-system-directory ".."))
    unboxed-system-directory)
  "The parent of the system directory if it is version-specific, or
the system directory otherwise" 
  :type 'directory
  :group 'unboxed-system)
(defcustom unboxed-system-archive-directory
  (file-name-concat unboxed-system-directory "site-elpa")
  "The directory in which unpacked archives for system packages are
found" 
  :type 'directory
  :group 'unboxed-system)
(defcustom unboxed-system-library-directory
  (file-name-concat unboxed-system-directory "site-lisp" "packages")
  "The directory in which unboxed elisp libraries for system packages
will be installed" 
  :type 'directory
  :group 'unboxed-system)
(defcustom unboxed-system-themes-directory
  (file-name-concat user-emacs-directory "site-themes")
  "The directory in which unboxed theme files for system packages will
be installed" 
  :type 'directory
  :group 'unboxed-system)
(defcustom unboxed-system-info-directory
  (file-name-concat user-emacs-directory "site-info")
  "The directory in which unboxed info files for system packages will
be installed" 
  :type 'directory
  :group 'unboxed-system)
(defcustom unboxed-system-data-directory
  (file-name-concat user-emacs-directory "site-data")
  "The directory in which package data directories for unboxed user
packages will be installed." 
  :type 'directory
  :group 'unboxed-system)

(defcustom unboxed-theme-libraries nil
  "List of elisp libraries for themes that are named `*-theme' but are
not themes themselves. These are required to be on the load-path
rather than in a theme directory." 
  :type '(repeat symbol)
  :group 'unboxed)

(defcustom unboxed-excluded-packages nil
  "List of packages that should never be managed by unbox regardless
  of the result of the predicate. Useful for packages that fail when
  unboxed for some reason, or for packages which the user wishes to
  maintain in traditional form, e.g. for active development of a
  package directory that is a git repo." 
  :type '(repeat symbol)
  :group 'unboxed)

(defcustom unboxed-package-data-directory-variables nil
  "Association list of packages mapped to the  file and variable names
that should be hard-coded to the data directory of the unboxed
  package.  There may be multiple associations for a given package" 
  :type '(repeat (list (symbol :tag "package")
		       (file :tag "library name")
		       (symbol :tag "data directory variable")))
  :group 'unboxed)

(defcustom unboxed-package-patches nil
  "Association list of packages mapped to a patch file making any
updates required to the package to make it compatible with unboxed
installation."  
  :type '(repeat (list (symbol :tag "package")
		       (file :tag "patch file")
		       (choice (const nil :tag "Default patch level 0")
			       (list (integer :tag "patch level")))))
  :group 'unboxed)

;;; libraries to install in the package lisp directory
(defun unboxed-library-p (path)
  "Predicate for elisp libraries contained in packages that should be
installed in the unboxed library directory. This predicate only
recognizes files matching `*.el' in top directory of the package
archive, and excludes `*-pkg.el' and `*-autoloads.el' files, since the
former are not proper elisp and the latter are not useful in an
unboxed installation." 
  (let ((ext (file-name-extension path))
	(base (file-name-nondirectory path))
	(dir (file-name-directory path)))
    (and ext (string= ext "el")
	 (not dir)
	 (or (not (string-suffix-p "-theme" base))
	     (memq (intern base) unboxed-theme-libraries))
	 (not (string-suffix-p "-package" base))
	 (not (string-suffix-p "-autoloads" base)))))

;;; modules to install in the package lisp directory
(defun unboxed-module-p (path)
  "Predicate for elisp modules contained in packages that should be
installed in the unboxed library directory." 
  (let ((ext (file-name-extension path))
	(base (file-name-nondirectory path))
	(dir (file-name-directory path)))
    (and ext (string= ext "so"))))

;;; theme files
(defun unboxed-theme-p (path)
  "Predicate for elisp libraries contained in packages that should be
installed in the unboxed theme directory. Any `*-theme.el' file whose
feature name is not contained in unboxed-theme-libraries variable is
classified as a theme."
  (let ((ext (file-name-extension path))
	(base (file-name-nondirectory path))
	(dir (file-name-directory path)))
    (and ext (string= ext "el")
	 (string-suffix-p "-theme" base)
	 (not (memq (intern base) unboxed-theme-libraries)))))

;;; info files
(defun unboxed-info-p (path)
  "Predicate for files contained in packages that should be installed
in the unboxed info directory. This predicate recognizes all `*.info'
and 'dir' files as info files"
  (let ((ext (file-name-extension path)))
    (or (and ext (string= ext "info"))
	(string= (file-name-nondirectory path) "dir"))))

;;; files to ignore
(defun unboxed-compiled-elisp-p (path)
  "Predicate for files contained in packages that should be installed
in the unboxed info directory. This predicate recognizes all `*.info'
and 'dir' files as info files"
  (let ((ext (file-name-extension path)))
    (and ext (string= ext "elc"))))


;;; Anything else
(defun unboxed-data-p (path)
  "Predicate for files contained in packages that should be installed
in the unboxed data directory for the package,
`<unboxed-data-directory>/<package-name>'.  This predicate is expected
to be executed last and just returns true."
  t)

;;; These installers are used for lists of files of a particular
;;; category for a specific package
(defun unboxed-install-theme (pd loc files)
  
  )
(defun unboxed-install-library (pd loc files)
  )
(defun unboxed-install-module (pd loc files)
  )
(defun unboxed-install-info (pd loc files)
  )
(defun unboxed-install-data (pd loc files)
  )
(defun unboxed-install-theme (pd loc files)
  
  )

;;; these functions are run with all the installed-file structs
;;; produced by a set of packages, after the above installers
;;; have completed for *all* the packages in that set.
;;; 
(defun unboxed-finalize-install-library (files)
  )
(defun unboxed-finalize-install-module (files)
  )
(defun unboxed-finalize-install-info (files)
  )
(defun unboxed-finalize-install-data (files)
  )
(defun unboxed-finalize-install-theme (files)
  
  )

(defcustom unboxed-user-categories
  `((theme unboxed-theme-p unboxed-user-theme-directory unboxed-install-theme unboxed-finalize-install-theme)
    (library unboxed-library-p unboxed-user-library-directory unboxed-install-library unboxed-finalize-install-library)
    (module unboxed-module-p unboxed-user-library-directory unboxed-install-module unboxed-finalize-install-module)
    (info unboxed-info-p unboxed-user-info-directory unboxed-install-info unboxed-finalize-install-info)
    (ignore unboxed-compiled-elisp-p nil nil nil)
    (data unboxed-data-p unboxed-user-data-directory unboxed-install-data unboxed-finalize-install-data))
  "Categories which determine where a file will be installed for user packages.
A file will be classified according to the first predicate returning a non-nil value,
with the order of testing determined by this list."
  :type '(repeat (list (symbol :tag "category")
		       (function :tag "predicate")
		       (choice (variable :tag "location")
			       (const :tag "none" nil))
		       (choice (function :tag "install-file")
			       (const :tag "none" nil))))
  :group 'unboxed)

(defcustom unboxed-system-categories
  `((theme unboxed-theme-p unboxed-system-theme-directory unboxed-install-theme unboxed-finalize-install-theme)
    (library unboxed-library-p unboxed-system-library-directory unboxed-install-library unboxed-finalize-install-library)
    (module unboxed-module-p unboxed-system-library-directory unboxed-install-module unboxed-finalize-install-module)
    (info unboxed-info-p unboxed-system-info-directory unboxed-install-info unboxed-finalize-install-info)
    (ignore unboxed-compiled-elisp-p nil nil nil)
    (data unboxed-data-p unboxed-system-data-directory unboxed-install-data))
  "Categories which determine where a file will be installed for system packages.
A file will be classified according to the first predicate returning a non-nil value,
with the order of testing determined by this list."
  :type '(repeat (list (symbol :tag "category")
		       (function :tag "predicate")
		       (choice (variable :tag "location")
			       (const :tag "none" nil))
		       (choice (function :tag "install-files")
			       (const :tag "none" nil))
		       (choice (function :tag "finalize-install-files")
			       (const :tag "none" nil))))
  :group 'unboxed-system)

;;  Currently the only option is a simple lisp object representation
;;  Eventually should use a sqlite format.  Taking this approach to avoid
;;  migration issues.
(defcustom unboxed-database-format
  "Format of package database file"
  :type '(choice (const :tag "Simple LISP object" sexpr))
  :group 'unboxed)

(define-error 'unboxed-invalid-category-spec
  "One or more fields in a file category specification is invalid")

(defvar unboxed--packages-to-install
  "List of packages to be unboxed once they are available as ordinary
packages in the package directory"
  nil)

(defvar unboxed--packages-to-remove
  "List of unboxed packages to be removed before removing the ordinary
packages in the package directory"
  nil)

(defvar unboxed--packages-to-rebox
  "List of unboxed packages to be put back under the standard package
installation management"
  nil)

(defun unboxed--categories-setting->struct (cat pred-name dir-var install-name)
  (let ((pred (and (fboundp pred-name) (symbol-function pred-name)))
	(dir (and (boundp dir-var) (symbol-value dir-var)))
	(install (and (fboundp install-name) (symbol-function install-name))))
    (unless (and pred dir install)
      (signal 'unboxed-invalid-category-spec
	      `(,cat (,pred-name ,pred)
		     (,dir-var ,dir)
		     (,install-name ,install))))
    (unboxed--file-category-create cat pred dir install)))

(cl-defstruct (unboxed--file-category
               (:constructor unboxed--file-category-create))
  "Structure for contents of package and each is installed.
  Slots:
  `name' name of file category as symbol
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
         updated autoloads."
  name
  predicate
  location
  install-file)

(cl-defstruct (unboxed-installed-file
               (:constructor unboxed-installed-file-create))
  "Structure for a file that is installed for a package
  Slots:
  `package' name of package as symbol
  `file' name of file as string
  `category' category of file as symbol
  `pkg-path' location of file in directory of package contents
  `install-path' location of file relative to the location of its category."
  package
  file
  category
  pkg-path
  install-path)

(cl-defstruct (unboxed--package-desc-layout
               (:constructor unboxed--package-desc-layout-create))
  "Record of the package-desc struct layout for instantiating structs
from a file. This allows for updates in package.el that may change the
layout of the struct in the current session.
  Slots:
  `seq-type' Value of (cl-struct-sequence-type 'package-desc)
  `slot-info' Value of (cl-struct-slot-info 'pacakge-desc)"
  seq-type
  slot-info)

(cl-defstruct (unboxed--package-desc
               (:constructor unboxed--package-desc-create)
	       (:include package-desc))
  "Package desc structure extended with fields recording its
installation manager 
  Slots:
  `manager' name of installation manager for this package."
  manager)

(cl-defstruct (unboxed--sexpr-db
	       (:constructor unboxed--sexpr-db-create))
  "Structure holding the tables of data for unboxed in sexpr db representation"
  categories
  package-desc-layout
  packages
  installed
  installed-by-cat)

(defun unboxed--is-user-package (pd)
  (let ((d (package-desc-dir pd)))
    (and d
	 (stringp d)
	 (string-prefix-p package-user-dir d))))

(defun unboxed--is-system-package (pd)
  (let ((d (package-desc-dir pd)))
    (and d
	 (stringp d)
	 (seq-filter (lambda (p) (string-prefix-p p d))
		     package-directory-list))))


(defun unboxed--init-package-desc (mgr pd)
  "Initialize an unboxed package descriptor from the manager field and a pre-existing package-desc"
  (let ((s (unboxed--package-desc-create :manager 'package))
	n)
    (setq n (length pd))
    (if (recordp s)
	(let ((i 1))
	  (while (< i n)
	    (aset s i (aref pd i))
	    (cl-incf i)))
      (let ((i0 1))
	(unless (eq (seq-elt s 0) 'unboxed--package-desc)
	  (setq i0 0))
	(seq-map-indexed (lambda (elt idx)
			   (when (>= idx i0)
			     (setf (seq-elt s idx) elt)))
			 pd)))))


(defun unboxed--create-sexpr-db (type)
  "Create an unboxed db in the sexpr format - initialize from package-desc table assuming no packages have been unboxed"
  (let (cats pdlo pkgs inst inst-by-cat)
    (setq cats
	  (mapcar #'(lambda (setting)
		      (let ((file-cat (apply #'unboxed--categories-setting->struct setting)))
			`(,(unboxed--file-category-name file-cat) . ,file-cat)))
		  (if (eq type 'system)
		      unboxed-system-categories
		    unboxed-user-categories)))
    (setq pdlo
	  (unboxed--package-desc-layout-create
	   (cl-struct-sequence-type 'package-desc)
	   (cl-struct-slot-info 'pacakge-desc)))
    (setq pkgs (make-hash-table :test #'eq))
    (mapc (lambda (pd)
	    (let ((upd (unboxed--init-package-desc 'package pd)))
	      (puthash pkgs (unboxed--package-desc-name upd) upd)))
	  (seq-filter (if (eq type 'system)
			  #'unboxed--is-system-package
			#'unboxed--is-user-package)
		      package-alist))
    ;; these record installed files managed by unboxed, so they start out empty
    (setq inst (make-hash-table :test #'equal))
    (setq inst-by-cat (make-hash-table :test #'eq))
    (unboxed--sexpr-db-create
     :categories cats
     :package-desc-layout pdlo
     :packages pkgs
     :installed inst
     :installed-by-cat inst-by-cat)))

(defcustom unboxed-data-directory-patterns
  '()
  "A list of pcase patterns that match against known expressions used to compute a packages installation directory
at either compile or load time.  Any matches will be hard-coded to be the value
of the unboxed package's data directory as a string."
  :type '(repeat (sexpr :tag "pcase pattern"))
  :group unboxed
  :setter #'unboxed--set-pcase-replace-sexpr-fxn
  )
(define-error 'unboxed-replace-sexpr "Bad sexpr parse")

(defun unboxed--set-pcase-replace-sexpr-fxn (patterns)
  (let* ((subst-var (make-symbol "value"))
	 (sexpr-var (make-symbol "sexpr"))
	 (clauses (mapcar (lambda (pattern)
			    `(,pattern (list ,subst-var)))
			  patterns))
	 (defun-expr
	   `(defun unboxed--pcase-replace-sexpr-p (,sexpr-var  ,subst-var) 
	      (pcase ,sexpr-var
		,@clauses
		(_ nil)))))
    (with-temp-buffer
      (prin1 defun-expr (current-buffer))
      (terpri (current-buffer))
      (goto-char 0)
      ;; compile the defun and install it - do not display the result in the echo area
      (compile-defun t))))
    
    
;;  This predicate function will be redefined by the setter for the
;;  pcase patterns for matching and replacing sexprs constructed by
;;  setter of the customization variable 
;;  if the sexpr is a match, the return value is a one-element list
;;  containing the replacement value
(defun unboxed--pcase-replace-sexpr-p (sexpr replacement) nil)

;; assumes parse-sexp-ignore-comments is t
(defun unboxed--back-sexpr (v p0)
  (let ((p2 (point))
	p1)
    ;; check for named unicode character that is not handled well by
    ;; emacs-lisp-mode (forward-sexp -1) 
    (when (and (char-or-string-p v)
	       (not (stringp v))
	       (eq (preceding-char) ?\}))
      ;; assume a unicode character name cannot contain a comment
      (save-excursion
	(setq p1 (search-backward "{" p0 t))
	(when p1
	  ;; this is only valid if it occured as a named unicode character
	  (setq p1 nil)
	  (and (looking-back "\\?\\\\N")
	       (forward-char -3)
	       (setq p1 (point))))))
    ;; (forward-sexp -1) also does not deal well with ## (the empty symbol)
    (unless (or p1 (not (and (symbolp v) (eq (preceding-char ?\#)))))
      (forward-char -1)
      (and (> (point) p0)
	   (eq (preceding-char ?\#))
	   (forward-char -1)
	   (setq p1 (point))))
    ;; #N# references are dealt with directly when read error is detected
    ;; assume other cases fairly well-behaved
    (unless p1
      (forward-sexp -1)
      (cond
       ((recordp v)
	(when (and (>= (point) (+ p0 2))
		   (eq (preceding-char) ?s)
		   (eq (char-before (- (point) 1)) ?\#))
	  (forward-char -2)))
       (t nil))
      (setq p1 (point)))
    (goto-char p1)
    p1))
       
(defun unboxed--replace-text-in-region (start end new)
  "Utility function for replacing region with specified string.
If point is inside the region, it will be at the end of the region following the replacement"
  (if (and (< (point) end) (> (point start)))
      (goto-char end))
  (save-excursion
    (goto-char end)
    (delete-region start end)
    (insert new)))

;; assumes preceding character is \#
(defun unboxed--check-invalid-read-graph-occurence (pos0)
  "Utility function for checking if reader failed due to encountering bare #N#"
  (let ((p2 (point))
	p0 p1 N retval)
    (save-excursion
      (forward-char -1)
      (setq p1 (point))
      (search-backward "#" pos0 t)
      (when (< (point) p1)
	(setq p0 (point))
	(forward-char 1)
	(ignore-error
	    (setq N (read (current-buffer))))
	(when (and N (natnump N) (>= (point) p1))
	  (setq retval p0))))
    retval))

;; assumes preceding character is either \) or \]
(defun unboxed--check-invalid-non-atomic (pos0)
  "Utility function for checking if reader failed due to encountering a non-atomic
sexpr containing a #N#."
  (let ((p2 (point))
	(close (preceding-char))
	open p1)
    (setq open
	  (if (eq close ?\])
	      "["
	    "("))
    (save-excursion
      (setq p1 (unboxed--back-sexpr nil pos0))
      ;; not infallibly correct, but good enough for non-pathological cases
      (unless (search-forward open p2 t)
	(setq p1 nil))
      p1)))

(defun unboxed--check-invalid-read (pos0)
  (let (p1)
    (cond
     ((eq (preceding-char ?\#)) ; check for #N#
      (setq p1 (unboxed--check-invalid-read-graph-occurence pos0)))
     ((or (eq (preceding-char ?\)))
	  (eq (preceding-char ?\]))) ; check for non-atomic
      (setq p1 (unboxed--check-invalid-non-atomic pos0))))
    p1))

;;  unboxed--pcase-replace-next-sexpr calls read from point to skip
;;  any comments and get the value represented by the text for
;;  testing. 
;;  It then attempts to identify the beginning of the textual
;;  representation using unboxed--back-sexpr before testing for
;;  replacement.
;;  We assume the (possibly narrowed) current buffer contains a valid
;;  elisp program. Given that, read will still signal an error in two
;;  cases: 
;;  1) When the EOF is reached - read has no other mechanism for
;;     indicating all expressions have been read
;;  2) Due to syntax that is valid as a subexpression of some sexpr,
;;     but not as a stand-alone expression.  
;;     a) Graph notation #N# represents an *occurrence* of an object
;;        represented elsewhere.  Hence we treat the traversal of this
;;        notation as a successful reading of the corresponding
;;        expression, although this prevents pcase testing from being
;;        performed on any containing sexpr 
;;     b) Graph notation #N= defining a graph object.  In this case we
;;        attempt to read the following object since the #N= does not
;;        itself correspond to any object occurrence. 
;;     c) The dot in a dotted pair representation of a cons cell. In
;;        this case we attempt to read the following sexpr as the dot
;;        does not correspond to any constructed object.
;;     d) Any non-atomic object failing due to containing an undefined
;;        #N# notation, though such a definition must have occurred if
;;        the top-level expression was successfully read.
;;  Note the lack of recording graph notation values means that pcase
;;  testing is not truly done *inside* circular objects in the elisp
;;  source code. 
;;
;;  In this implementation, there are 3 main positions of concern:
;;  position 0 - the initial value of point before calling read
;;  position 2 - the position immediately following the last character
;;               of an expression
;;  position 1 - the position immediately preceding the first
;;               character of the text representing the value which is
;;               going to be tested and possibly replaced 
;;
;;  Every position 0 is either the beginning of the buffer or position
;;  2 of some processed value.

(defun unboxed--pcase-replace-next-sexpr ()
  "Perform matching/replacement in the region containing the first
  sexpr found in the current buffer following the point in the current
  buffer. The search will recurse if the sexpr is a list or vector."
  (let ((pos0 (point))
	(read-attempts 0)
	eof pos1 pos2 v m retval)
    (while (and (not pos2) (not eof) (< read-attempts 2))
      (condition-case nil
	  ;; this will error if there are no additional expressions found
	  (setq v (read (current-buffer))
		pos2 (point))
	(invalid-read-syntax
	 (let ((p1 (unboxed--check-invalid-read pos0)))
	   (when p1
	     (setq pos1 p1
		   pos2 (point)))))
	(error (setq eof t)))
      (cl-incf read-attempts))
    (when (and pos2 (not pos1))
      ;; could push this into the cond clause for a successful
      ;; match *if* matcher was guaranteed to have no side-effects
      (setq pos1 (unboxed--back-sexpr v pos0 pos2)
	    m (unboxed--pcase-replace-matching-sexpr v))
      (goto-char pos2)
      (cond
       (m (unboxed--replace-text-in-region pos1 pos2 (car m)))
       ((atom v) nil)
       ((or (listp v) (recordp v) (arrayp v))
	(save-excursion
	  (goto-char (scan-lists pos1 1 -1))
	  (unboxed--pcase-replace-in-seq 0 (length v))
	  (when (<= (point) pos1)
	    (signal unboxed-replace-sexpr `[,v ,pos0 ,pos1 ,pos2 ,(point)]))))))
    (not eof)))

(defun unboxed--pcase-replace-in-seq (i n)
  (while (< i n)
    (unboxed--pcase-replace-next-sexpr)
    (cl-incf i)))

;; search text in current buffer for sexprs matching one of a supplied set of pcase patterns
;; if a match is found, replace the text with corresponding sexpr value
;; Replacement should not disturb relative position of the text surrounding the text
;; that produced the sexpr matching the supplied pcase pattern
(defun unboxed--pcase-replace-sexpr ()
  "Simple search and replace on sexprs to match common expressions
   used in defining data directory variables for packages."
  (emacs-lisp-mode)
  (let ((parse-sexp-ignore-comments t))
    (save-excursion
      (goto-char (point-min))
      (while (unboxed--pcase-replace-next-sexpr)))))
    
(provide 'unboxed)
;;; unboxed.el ends here

;; 

