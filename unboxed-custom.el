;;; unboxed-custom.el        -*- lexical-binding: t; -*-

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

;; Customization variables and supporting code for unboxed package management

;;; Code:

(require 'unboxed-decls)


(defgroup unboxed nil
  "Manager for unboxed Emacs Lisp packages."
  :group 'package
  :version "24.1")

(defgroup unboxed-site nil
  "Settings for unboxed Emacs Lisp packages installed in site area"
  :group 'unboxed
  :version "24.1")
(defgroup unboxed-user nil
  "Settings for unboxed Emacs Lisp packages installed in user area"
  :group 'unboxed
  :version "24.1")

(defcustom unboxed-user-db-path user-emacs-directory
  "Directory in which the database tracking installed user packages
will be stored" 
  :type 'directory
  :group 'unboxed-user)
(defcustom unboxed-user-db-file "packages.sqlite"
  "File name of database tracking installed user packages"
  :type 'filename
  :group 'unboxed-user)
(defcustom unboxed-site-db-path data-directory
  "Directory in which the database tracking installed system packages
will be stored" 
  :type 'directory
  :group 'unboxed-site)
(defcustom unboxed-site-db-file "system-packages.sqlite"
  "File name of database tracking installed system packages"
  :type 'filename
  :group 'unboxed-site)

(defcustom unboxed-user-package-archive package-user-dir
  "Directory in which unpacked user packages are found"
  :type 'directory
  :group 'unboxed-user)
(defcustom unboxed-user-library-directory
  (file-name-concat user-emacs-directory "lisp")
  "Directory in which unboxed elisp libraries from user packages will
be installed.  Will be added to the load-path." 
  :type 'directory
  :group 'unboxed-user)
(defcustom unboxed-user-themes-directory
  (file-name-concat user-emacs-directory "themes")
  "Directory in which unboxed theme files from user packages will be
installed." 
  :type 'directory
  :group 'unboxed-user)
(defcustom unboxed-user-info-directory
  (file-name-concat user-emacs-directory "info")
  "Directory in which unboxed info files from user packages will be
installed." 
  :type 'directory
  :group 'unboxed-user)
(defcustom unboxed-user-data-directory
  (file-name-concat user-emacs-directory "data")
  "The directory in which package data directories for unboxed user
packages will be installed." 
  :type 'directory
  :group 'unboxed-user)

(defcustom unboxed-temp-directory
  (file-name-concat user-emacs-directory "tmp")
  "Directory for temporary files created during unboxed package
operations.  An example would be the compilation logs for an 
asynchronously byte-compiled library." 
  :type 'directory
  :group 'unboxed)

(defcustom unboxed-site-directory
  (expand-file-name (file-name-concat data-directory ".."))
  "The version-specific directory in which emacs is installed"
  :type 'directory
  :group 'unboxed-site)

(defcustom unboxed-site-unversioned-directory
  (if (string= (file-name-nondirectory unboxed-site-directory)
	       (format "%s.%s" emacs-major-version emacs-minor-version))
      (expand-file-name (file-name-concat unboxed-site-directory ".."))
    unboxed-site-directory)
  "The parent of the system directory if it is version-specific, or
the system directory otherwise" 
  :type 'directory
  :group 'unboxed-site)

(defcustom unboxed-site-archive-directory
  (file-name-concat unboxed-site-directory "site-elpa")
  "The directory in which unpacked archives for system packages are
found" 
  :type 'directory
  :group 'unboxed-site)

(defcustom unboxed-site-library-directory
  (file-name-concat unboxed-site-directory "site-lisp" "packages")
  "The directory in which unboxed elisp libraries for system packages
will be installed" 
  :type 'directory
  :group 'unboxed-site)

(defcustom unboxed-site-themes-directory
  (file-name-concat user-emacs-directory "site-themes")
  "The directory in which unboxed theme files for system packages will
be installed" 
  :type 'directory
  :group 'unboxed-site)

(defcustom unboxed-site-info-directory
  (file-name-concat user-emacs-directory "site-info")
  "The directory in which unboxed info files for system packages will
be installed" 
  :type 'directory
  :group 'unboxed-site)

(defcustom unboxed-site-data-directory
  (file-name-concat user-emacs-directory "site-data")
  "The directory in which package data directories for unboxed user
packages will be installed." 
  :type 'directory
  :group 'unboxed-site)

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


(defcustom unboxed-install-info-program "install-info"
  "Path to install-info program"
  :type 'file
  :group 'unboxed)

;;  Currently the only option is a simple lisp object representation
;;  Eventually should use a sqlite format.  Taking this approach to avoid
;;  migration issues.
(defcustom unboxed-database-format
  "Format of package database file"
  :type '(choice (const :tag "Simple LISP object" sexpr))
  :group 'unboxed)

(defconst unboxed--default-user-categories
  `((theme unboxed-theme-p unboxed-user-theme-directory
	   unboxed-install-theme unboxed-finalize-install-theme
	   unboxed-remove-theme unboxed-finalize-remove-theme)
    (library unboxed-library-p unboxed-user-library-directory
	     unboxed-install-library unboxed-finalize-install-library
	     unboxed-remove-library unboxed-finalize-remove-library)
    (module unboxed-module-p unboxed-user-library-directory
	    unboxed-install-module unboxed-finalize-install-module
	    unboxed-remove-module unboxed-finalize-remove-module)
    (info unboxed-info-p unboxed-user-info-directory
	  unboxed-install-info unboxed-finalize-install-info
	  unboxed-remove-info unboxed-finalize-remove-info)
    (ignore unboxed-compiled-elisp-p nil nil nil nil nil)
    (data unboxed-data-p unboxed-user-data-directory
	  unboxed-install-data unboxed-finalize-install-data
	  unboxed-remove-data unboxed-finalize-remove-data)))

(defconst unboxed-default-site-categories
  `((theme unboxed-theme-p unboxed-site-theme-directory
	   unboxed-install-theme unboxed-finalize-install-theme
	   unboxed-remove-theme unboxed-finalize-remove-theme)
    (library unboxed-library-p unboxed-site-library-directory
	     unboxed-install-library unboxed-finalize-install-library
	     unboxed-remove-library unboxed-finalize-remove-library)
    (module unboxed-module-p unboxed-site-library-directory
	    unboxed-install-module unboxed-finalize-install-module
	    unboxed-remove-module unboxed-finalize-remove-module)
    (info unboxed-info-p unboxed-site-info-directory
	  unboxed-install-info unboxed-finalize-install-info
	  unboxed-remove-info unboxed-finalize-remove-info)
    (ignore unboxed-compiled-elisp-p nil nil nil nil nil)
    (data unboxed-data-p unboxed-site-data-directory
	  unboxed-install-data unboxed-finalize-install-data
	  unboxed-remove-data unboxed-finalize-remove-data)))

(defcustom unboxed-data-directory-patterns
  '()
  "A list of pcase patterns that match against known expressions used to compute a packages installation directory
at either compile or load time.  Any matches will be hard-coded to be the value
of the unboxed package's data directory as a string."
  :type '(repeat (sexpr :tag "pcase pattern"))
  :group unboxed
  :setter #'unboxed--set-pcase-replace-sexpr-p
  )

(defun unboxed--set-pcase-replace-sexpr-p (patterns)
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
      ;; compile the defun and install it - do not display the result
      ;; in the echo area 
      (compile-defun t))))
    
    
;;  This predicate function will be redefined by the setter for the
;;  pcase patterns for matching and replacing sexprs constructed by
;;  setter of the customization variable 
;;  if the sexpr is a match, the return value is a one-element list
;;  containing the replacement value
(defun unboxed--pcase-replace-sexpr-p (sexpr replacement) nil)

(provide 'unboxed-custom)

;;; unboxed-custom.el ends here

;; 

