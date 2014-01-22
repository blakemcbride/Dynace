

(define delete-file2
  (lambda (f)
    (with-handlers ([exn:i/o:filesystem? 
		     (lambda (exn) #f)])
		   (delete-file f)
		   #t)))

(define copy-file2
  (lambda (f t)
    (with-handlers ([exn:i/o:filesystem? 
		     (lambda (exn) #f)])
		   (if (file-exists? t)
		       (delete-file2 t))
		   (copy-file f t)
		   #t)))

(define rename
  (lambda (f t)
    (with-handlers ([exn:i/o:filesystem? 
		     (lambda (exn) #f)])
		   (rename-file-or-directory f t)
		   #t)))


(if
 (rename "wds2.exe" "wds3.exe")
  (gMessage Application "Yes")
  (gMessage Application "No"))
