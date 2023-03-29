;;; unboxed.el --- Unboxed package management        -*- lexical-binding: t; -*-

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

;; Unboxed installs packages in a single "packages" directory whenever
;; possible.
;;
;; The update of autoloads and byte-compiling steps for elisp libraries are
;; performed once per set of packages, rather than recompiling the
;; quickstart file for each individual package installed.  When a
;; large number of packages are installed, this is much more
;; efficient.
;;
;; Theme and info files are installed in dedicated locations rather
;; than growing the associated path variable.

;;; Code:

(require 'package)
(require 'cl-lib)
(require 'seq)
(require 'async)
(require 'unboxed-decls)
(require 'unboxed-categories)
(require 'unboxed-file-management)
(require 'unboxed-custom)
(require 'unboxed-rewrite-sexprs)
(require 'unboxed-database)

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

(defvar unboxed--areas nil)

    
(provide 'unboxed)
;;; unboxed.el ends here

;; 

