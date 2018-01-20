#lang racket
(require "buffer.rkt")
(require "low-level.rkt")

(require zippers)
(require lens)

(define box-lens (make-lens unbox set-box!))

; TODO: use frp rather than boxes
(define *buffer* (box (make-buffer)))
(define *cursor* (box '(0 . 0)))

(define (current-line)
  (zipper-focus ((down/line-ref (current-lineno)) (unbox *buffer*))))

(define (current-lineno)
  (car (unbox *cursor*)))

(define (current-linepos)
  (cdr (unbox *cursor*)))

(define (make-safe-move! m)
  (when (can-move? m (unbox *buffer*))
    (set-box! *buffer* (m (unbox *buffer*)))
    #t)
  #f)

(define (cursor-move! dir)
  (let ((new-cursor
          (case dir
            ((up)
             (lens-transform car-lens (unbox *cursor*) sub1))
            ((down)
             (lens-transform car-lens (unbox *cursor*) add1))
            ((left)
             (lens-transform cdr-lens (unbox *cursor*) sub1))
            ((right)
             (lens-transform cdr-lens (unbox *cursor*) add1)))))
    (when (make-safe-move! (down/char-ref (car new-cursor) (cdr new-cursor)))
      (set-box! *cursor* new-cursor))))

(define (key-handler win key scancode action mods)
  (when (eq? action glfw-press)
    (cond
      ((eq? key glfw-key-up)
       (cursor-move! 'up))
      ((eq? key glfw-key-down)
       (cursor-move! 'down))
      ((eq? key glfw-key-right)
       (cursor-move! 'right))
      ((eq? key glfw-key-left)
       (cursor-move! 'left))
      ((eq? key glfw-key-enter)
       ;(insert-line-after! (current-lineno))
       ;(cursor-down!)
       )
      ((eq? key glfw-key-backspace)
       ;(backspace!)
       ;(cursor-left!)
      ; (clamp-cursor!)
       ))
    ))

(define (char-handler win codept)
  (set-box!
    *buffer*
    (up (edit (const (string (integer->char codept))) ((down/char-ref (current-lineno) (current-linepos)) (unbox *buffer*)))))
  (cursor-move! 'right))

(define win (new window-class% (char-handler char-handler) (key-handler key-handler)))
(send win render-loop
      (λ ()
         (for ((l (rebuild (unbox *buffer*)))
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
