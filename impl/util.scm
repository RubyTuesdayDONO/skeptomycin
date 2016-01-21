

(define nth 
  (lambda (index list)
    (list-ref list (1- index))))

(define second 
  (lambda (list)
    (nth 2 list)))

(define first
  (lambda (list)
    (nth 1 list)))



