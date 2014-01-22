(define (FormatDate ldt fmt)
  (let* ((dobj (gNewWithLong Date ldt))
	 (sobj (gFormatDate dobj fmt))
	 (str  (gStringValue sobj)))
    (gDispose dobj)
    (gDispose sobj)
    str))

(define (StripCenter str)
  (Cs str))

(define (FindStr s f ic)
  (let* [(slen (string-length s))
	 (flen (string-length f))
	 (ss   "")
	 (si   0)
	 (rval #f)]

    (while (and (not rval) (<= si (- slen flen)))
	   (set! ss (substring s si (+ si flen)))
	   (if (or (and ic (string-ci=? ss f))
		   (and (not ic) (string=? ss f)))
	       (set! rval si)
	       (x+ si)))
	   
    rval))

(define (Replace f t ic s)
  (let* [(rs  s)
	 (si  (FindStr s f ic))
	 (len (string-length f))]

    (while si
	   (set! rs (string-append (substring rs 0 si)
				   t
				   (substring rs (+ si len) (string-length rs))))
	   (set! si (FindStr rs f ic)))
    rs))

(define ReplacePairs
  (lambda (str . pair-list)
    (let* [(rval str)]
      (let loop [(ls pair-list)]
	(cond [(> (length ls) 1)
	       (set! rval (Replace (string-append "#" (car ls) "#") (cadr ls) #f rval))
	       (loop (cddr ls))]))
      rval)))

