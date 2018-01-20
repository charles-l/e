#lang debug racket

; TODO: performance tune the crap outta this vector stuff

(require zippers)
(require srfi/13)

(define (make-buffer)
  (zip '("")))


(define (last-vector-index v)
  (sub1 (vector-length v)))

#;(define (insert buffer contents))

(define (buffer-before buffer y (x 0))
  (let ((lines-before (vector-take buffer y)))
    (if (<= x 0)
      (vector-append
        lines-before
        #(""))
      (vector-append
        lines-before
        (vector-immutable (string-take (vector-ref buffer y) x))))))

(define (buffer-after buffer y (x (sub1 (string-length (vector-ref buffer y)))))
  (let ((lines-after (vector-drop buffer (add1 y))))
    (if (>= x (sub1 (string-length (vector-ref buffer y))))
      (vector-append #("") lines-after)
      (vector-append (vector-immutable (string-drop (vector-ref buffer y) (add1 x)))
                     lines-after))))

(struct buffer-item-frame (to-left to-right)
  #:property prop:zipper-frame
  (lambda (frame focus)
    (match frame
      ((buffer-item-frame l r)
       (vector-append (vector-drop-right l 1)
                      (vector-immutable
                        (string-append
                          (vector-ref l (last-vector-index l))
                          focus
                          (vector-ref r 0)))
                      (vector-drop r 1)))
      (else
        (error "should be a buffer-item-frame struct:" frame))))
  #:transparent)

(define (down/line-ref i)
  (zipper-movement
    (match-lambda
      ((zipper (? vector? v) context)
       (zipper (vector-ref v i)
               (cons (buffer-item-frame
                       (buffer-before v i)
                       (buffer-after v i))
                     context))))
    (lambda (z)
      (and (vector? (zipper-focus z))
           (< i (vector-length (zipper-focus z)))))))

(define (down/char-ref li i)
  (zipper-movement
    (match-lambda
      ((zipper (? vector? v) context)
       (zipper (substring (vector-ref v li) i (add1 i))
               (cons (buffer-item-frame
                       (buffer-before v li i)
                       (buffer-after v li i))
                     context))))
    (lambda (z)
      (and (vector? (zipper-focus z))
           (< li (vector-length (zipper-focus z)))
           (< i (string-length (vector-ref (zipper-focus z) li)))))))


(define b (zip #("a string"  "some stuff"  "a single line"  "c"  ""  "more")))

(edit (const "some letters ") ((down/char-ref 0 3) (up (edit (const "different line") ((down/line-ref 2) b)))))


#;(define (delete buffer))
