;  This file loads the appropriate library for compiling scheme code into
;  byte code (.zo) files

;  First edit the first two lines to make sure the correct path to the scheme
;  libraries is set.
;
;  Next go into mzscheme.exe and (load "schme-compiler.scm") to load this file
;
;  To compile a file do:  (compile-file "file.scm" "file.zo")
;
;  The resulting .zo file should be placed in a sub-directory called "compiled"
;  from where the .scm file normally resides.
;
;  Use (load/use-compiled "file.scm") to load the file.
;  This will really load "compiled/file.zo" if it exists and is a later date
;  than file.scm.  It will also use the .zo file if the .scm file is not there.




;(current-library-collection-paths (list (build-path (current-directory) "scheme")))
(current-library-collection-paths (list (build-path "D:\\Dynace.401\\scheme")))

(require-library "compile.ss")

