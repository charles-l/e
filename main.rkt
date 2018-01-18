#lang racket
(require "low-level.rkt")

(define *buffer* (vector ""))
(define *cursor* (box '(0 . 0)))

(define (current-lineno)
  (car (unbox *cursor*)))

(define (current-linepos)
  (cdr (unbox *cursor*)))

(define (current-line)
  (vector-ref *buffer* (current-lineno)))

(define (insert-line-after! i)
  (let* ((i (add1 (clamp0 i))) (a (vector-take *buffer* i)) (b (vector-drop *buffer* i)))
    (set! *buffer* (vector-append a #("") b))))

(define (backspace!)
  (vector-set!
    *buffer*
    (current-lineno)
    (string-join
      (list
        (substring
          (vector-ref *buffer* (current-lineno))
          0
          (clamp0 (- (current-linepos) 2)))
        (substring
          (vector-ref *buffer* (current-lineno))
          (sub1 (current-linepos))
          (string-length (vector-ref *buffer* (current-lineno)))))
      "")))

;;; clamp a value to >= 0
(define (clamp0 v)
  (max 0 v))

;;; clamp a value to >= 1
(define (clamp1 v)
  (max 1 v))

;;; clamp value to range [lb, ub)
(define (clamp-upto v ub (lb 0))
  (min (max lb v) (sub1 ub)))

;; lock the cursor back onto the "rail" of text, otherwise it
;; might sit outside of the valid range of positions in the buffer
(define (clamp-cursor!)
  (let* ((lineno (clamp-upto (current-lineno) (vector-length *buffer*)))
         (linepos (clamp-upto (current-linepos) (add1 (string-length (vector-ref *buffer* lineno))))))
    (set-box! *cursor* (cons lineno linepos))))

(define (cursor-up!)
  (set-box! *cursor* (cons (sub1 (current-lineno)) (current-linepos))))

(define (cursor-down!)
  (set-box! *cursor* (cons (add1 (current-lineno)) (current-linepos))))

(define (cursor-left!)
  (set-box! *cursor* (cons (current-lineno) (sub1 (current-linepos)))))

(define (cursor-right!)
  (set-box! *cursor* (cons (current-lineno) (add1 (current-linepos)))))

(define (key-handler win key scancode action mods)
  (when (eq? action glfw-press)
    (cond
      ((eq? key glfw-key-up)
       (cursor-up!)
       (clamp-cursor!))
      ((eq? key glfw-key-down)
       (cursor-down!)
       (clamp-cursor!))
      ((eq? key glfw-key-right)
       (cursor-right!)
       (clamp-cursor!))
      ((eq? key glfw-key-left)
       (cursor-left!)
       (clamp-cursor!))
      ((eq? key glfw-key-enter)
       (insert-line-after! (current-lineno))
       (cursor-down!)
       (clamp-cursor!))
      ((eq? key glfw-key-backspace)
       (backspace!)
       (cursor-left!)
       (clamp-cursor!)))
    ))

(define (char-handler win codept)
  (vector-set!
    *buffer*
    (current-lineno)
    (string-append (vector-ref *buffer* (current-lineno)) (~a (integer->char codept))))
  (set-box! *cursor* (cons (current-lineno) (add1 (current-linepos)))))

(define win (new window-class% (char-handler char-handler) (key-handler key-handler)))
(send win render-loop
      (λ ()
         (for ((l *buffer*)
               (i (in-naturals)))
           (send win draw-text
                 *padding*
                 (exact->inexact (* (add1 i) *line-height*))
                 l))
         (send win set-color 255 255 255 50)
         (send win draw-text 255.0 255.0 (~a (unbox *cursor*)))

         ; TODO: clean this mess up
         (let ((cursor-pixel-x
                 (+ *padding*
                    (if (< (current-linepos) (string-length (current-line)))
                      (send win calculate-character-x
                            (current-line)
                            (sub1 (current-linepos)))
                      (let ((l (sub1 (string-length (current-line)))))
                        (+ (send win calculate-character-x (current-line) l)
                           (* (sub1 (- (current-linepos) l)) (/ *font-size* 2)))))))
               (cursor-pixel-y
                 (exact->inexact (+ *padding* (* (current-lineno) *line-height*)))))
           (send win draw-rect
                 cursor-pixel-x
                 cursor-pixel-y
                 (/ *font-size* 2)
                 *font-size*))))
