

(define *tl:current-time* #f)

(define *tl:begining-time* #f)

(define *tl:port* #f)

(define tl:start-timer
  (lambda (file)
    (tl:end-timer)
    (set! *tl:port* (open-output-file file 'text 'truncate))))

(define tl:end-timer
  (lambda ()
    (cond (*tl:port*
	   (close-output-port *tl:port*)
	   (set! *tl:port* #f)))
    (set! *tl:begining-time* #f)
    (set! *tl:current-time* #f)))

(define tl:mark-time
  (lambda (msg)
    (let ((last *tl:current-time*)
	  (fmt (lambda (n)
		 (Nfmt n "C" 0 3))))
      (set! *tl:current-time* (/ (current-milliseconds) 1000.0))
      (if (not *tl:begining-time*)
	  (set! *tl:begining-time* *tl:current-time*))
      (display (string-append msg " - ") *tl:port*)
      (if (not last)
	  (display "timer start" *tl:port*)
	  (begin
	    (display (fmt (- *tl:current-time* *tl:begining-time*)) *tl:port*)
	    (display "  " *tl:port*)
	    (display (fmt (- *tl:current-time* last)) *tl:port*)))
      (newline *tl:port*))))
	    
    