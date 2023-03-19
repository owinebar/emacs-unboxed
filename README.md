# emacs-unboxed
Package management tools to install packages "unboxed".

The standard package manager installs every package in its own directory.  
When installing many packages, this can make the load-path so long that performance 
is noticeably affected.  In most cases, this isolation is completely unnecessary.
The vast majority of packages are just single elisp files.  Eliminating the additional
entries in the load-path for these alone may be a significant improvement.  Aside from
these, many multi-file packages do not have any subdirectories.  The elisp files
for these packages can generally be safely installed in the same unboxed package directory.
For packages that are complicated enough to have subdirectories with either data or
additional elisp library files, most can be successfully unboxed with only minor changes 
to a single source file, where that file sets a variable with the path to the directory
containing the data or libraries.
Only a small number of packages are difficult to automatically adapt to an unboxed package
installation.  The primary problematic packages are those that are overyly clever in
the way they load packages, usually to support developers actively developing the package
while it is also in use.  The most significant packages with this issue are the ones
make use of the load-relative package, namely "realgud" and its derivatives.  However, if
these are the only packages that are not unboxed, the load path will not be ridiculously
large.

