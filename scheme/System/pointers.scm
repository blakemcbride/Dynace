
(define *null-pointer* '(null))

(define *pointers* (make-vector 100 *null-pointer*))

(define Make-Pointer
  (lambda (obj)
    (do ((i 0 (+ i 1)))
	((or (>= i (vector-length *pointers*))
	     (eq? (vector-ref *pointers* i) *null-pointer*))
	 (if (>= i (vector-length *pointers*))
	     0
	     (begin
	       (vector-set! *pointers* i obj)
	       (+ i 1)))))))

(define Deref-Pointer
  (lambda (ptr)
    (vector-ref *pointers* (- ptr 1))))

(define Free-Pointer
  (lambda (ptr)
    (vector-set! *pointers* (- ptr 1) *null-pointer*)
    0))

(define Change-Pointer
  (lambda (ptr val)
    (vector-set! *pointers* (- ptr 1) val)
    val))
