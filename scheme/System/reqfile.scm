(push-namespace 'RequireFile)

(define FileList null)

(define needToLoadFile
  (lambda (fname col)
    (let*[(fullName (if col
			(string-append (car (current-library-collection-paths))
				       "/" col "/" fname)
			fname))
	  (data (assoc fullName FileList))]
      
      (if data
	  (let* [(size (caadr data))
		 (modd (cadadr data))]
	    (cond [(or (not (= size (file-size fullName)))
		       (not (= modd (file-or-directory-modify-seconds fullName))))
		   #t]
		  [else
		   #f]))
	  #t))))

(define setReturnValue
  (lambda (fname col rval)
    (let*[(fullName (if col
			(string-append (car (current-library-collection-paths))
				       "/" col "/" fname)
			fname))
	  (data (assoc fullName FileList))]

      (cond [data
	     (set-car! (cadr data) (file-size fullName))
	     (set-car! (cdadr data) (file-or-directory-modify-seconds fullName))
	     (set-car! (cddadr data) rval)]
	    [else
	     (set! FileList (cons (list fullName
					(list (file-size fullName)
					      (file-or-directory-modify-seconds fullName)
					      rval))
				  FileList))]))))

(define getReturnValue
  (lambda (fname col)
    (let*[(fullName (if col
			(string-append (car (current-library-collection-paths))
				       "/" col "/" fname)
			fname))
	  (data (assoc fullName FileList))]

      (if data
	  (car (cddadr data))
	  null))))

(pop-namespace)
