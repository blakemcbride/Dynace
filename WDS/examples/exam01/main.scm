
(load "resources.scm")

(define (start)
  (let ((win nil))
    (set! win (gNewWithStr MainWindow "My Test Application"))
    (gLoadIcon win ALGOCORP_ICON)
    (gPuts win "Hello, World!")
    (gProcessMessages win)))


(start)



