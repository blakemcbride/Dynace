(define threadInit
	(lambda (threadobj)
		(thread (lambda () (gSchemeThreadWait threadobj)))))

(define threadFactoryInit
	(lambda ()
		(thread (lambda () (gCreateSchemeThreadWait SchemeThread)))))
