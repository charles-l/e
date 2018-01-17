#lang racket
(require "low-level.rkt")

(define *buffer* (vector ""))
(define *cursor* (box '(0 . 0)))

(define (current-lineno)
  (car (unbox *cursor*)))

(define (current-linepos)
  (cdr (unbox *cursor*)))

(define (new-line! i)
  (let-values (((a b) (vector-split-at *buffer* i)))
    (set! *buffer* (vector-append a #("") b)))
  (set-box! *cursor* (cons (add1 (current-lineno)) 0)))

(define (backspace! p)
  (vector-set!
    *buffer*
    (current-lineno)
    (string-join
      (list
        (substring
          (vector-ref *buffer* (current-lineno))
          0
          (current-linepos))
        (substring
          (vector-ref *buffer* (current-lineno))
          (add1 (current-linepos))
          (string-length (vector-ref *buffer* (current-lineno)))))
      "")))

(define (key-handler win key scancode action mods)
  (when (eq? action glfw-press)
    (cond
      ((eq? key glfw-key-enter)
       (new-line! (vector-length *buffer*)))
      ((eq? key glfw-key-backspace)
       (backspace! (cons (sub1 (vector-length *buffer*))
                         (sub1 (string-length (vector-ref *buffer* (sub1 (vector-length *buffer*)))))))))))

(define (char-handler win codept)
  (vector-set!
    *buffer*
    (current-lineno)
    (string-append (vector-ref *buffer* (current-lineno)) (~a (integer->char codept)))))

(define win (new window-class% (char-handler char-handler) (key-handler key-handler)))
(send win render-loop
      (λ ()
         (for ((l *buffer*)
               (i (in-naturals)))
           (send
             win
             text
             *padding*
             (exact->inexact (* (add1 i) *line-height*))
             l))
         (send
           win
           rect
           (+ *padding* (* *font-size* (current-linepos)))
           (exact->inexact (+ *padding* (* (current-lineno) *line-height*)))
           (/ *font-size* 2)
           *font-size*
           '(255 255 0 80))))
