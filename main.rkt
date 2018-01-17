#lang racket

(require ffi/unsafe
         ffi/unsafe/define)

(require racket/stream)

(define _nvg-color (_array _float 4))

(define-ffi-definer define-native (ffi-lib "./shared"))

(define _nvg-ptr (_cpointer 'NVGContext))
(define _WINDOW-ptr (_cpointer 'GLFWwindow))

(define-native init (_fun -> _WINDOW-ptr))
(define-native initVG (_fun _WINDOW-ptr -> _nvg-ptr))
(define-native glfwWindowShouldClose (_fun _WINDOW-ptr -> _bool))
(define-native glfwPollEvents (_fun -> _void))
(define-native glfwSwapBuffers (_fun _WINDOW-ptr -> _void))
(define-native glfwTerminate (_fun -> _void))
(define-native glUseProgram (_fun _uint -> _void))
(define-native nvgSave (_fun _nvg-ptr -> _void))
(define-native nvgRestore (_fun _nvg-ptr -> _void))
(define-native nvgMoveTo (_fun _nvg-ptr _float _float -> _void))
(define-native nvgBeginFrame (_fun _nvg-ptr _int _int _float -> _void))
(define-native nvgEndFrame (_fun _nvg-ptr -> _void))
(define-native nvgBeginPath (_fun _nvg-ptr -> _void))
(define-native nvgClosePath (_fun _nvg-ptr -> _void))
(define-native nvgRect (_fun _nvg-ptr _float _float _float _float -> _void))
(define-native nvgFill (_fun _nvg-ptr -> _void))
(define-native nvgFontSize (_fun _nvg-ptr _float -> _void))
(define-native nvgTextLetterSpacing (_fun _nvg-ptr _float -> _void))
(define-native setFill (_fun _nvg-ptr _uint8 _uint8 _uint8 _uint8 -> _void))
(define-native renderText (_fun _nvg-ptr _float _float _string -> _void))
(define-native clear (_fun -> _void))

(define-native glfwSetCharCallback (_fun _WINDOW-ptr (_fun _WINDOW-ptr _uint -> _void) -> _void))
(define-native glfwSetKeyCallback (_fun _WINDOW-ptr (_fun _WINDOW-ptr _int _int _int _int -> _void) -> _void))

(define glfw-key-enter 257)
(define glfw-key-backspace 259)
(define glfw-press 1)

(define *buffer* (vector ""))
(define *cursor* (box '(0 . 0)))

(define *font-size* 15.0)
(define *line-height* 15.0)
(define *padding* 5.0)


(define (current-line)
  (car (unbox *cursor*)))

(define (current-linepos)
  (cdr (unbox *cursor*)))

(define (new-line! i)
  (let-values (((a b) (vector-split-at *buffer* i)))
    (set! *buffer* (vector-append a #("") b)))
  (set-box! *cursor* (cons (add1 (current-line)) 0)))

(define (backspace! p)
  (vector-set!
    *buffer*
    (current-line)
    (string-join
      (list
        (substring
          (vector-ref *buffer* (current-line))
          0
          (current-linepos))
        (substring
          (vector-ref *buffer* (current-line))
          (add1 (current-linepos))
          (string-length (vector-ref *buffer* (current-line)))))
      "")))

(define (key-handler win key scancode action mods)
  (when (eq? action glfw-press)
    (cond
      ((eq? key glfw-key-enter)
       (new-line! (vector-length *buffer*)))
      ((eq? key glfw-key-backspace)
       (backspace! (cons (sub1 (vector-length *buffer*))
                         (sub1 (string-length (vector-ref *buffer* (sub1 (vector-length *buffer*)))))))))))

(define (vector-last-i v)
  (- (vector-length v) 1))

(define (char-handler win codept)
  (vector-set!
    *buffer*
    (vector-last-i *buffer*)
    (string-append (vector-ref *buffer* (vector-last-i *buffer*)) (~a (integer->char codept))))
  #;(moveCursor (exact->inexact (* +character-distance+ 0)) (+ 600.0 0))
  )

(let ((win (init)))
  (glfwSetCharCallback win char-handler)
  (glfwSetKeyCallback win key-handler)
  (let ((vg (initVG win)))
    (let loop ()
      (unless (glfwWindowShouldClose win)
        (glfwPollEvents)
        (clear)

        (nvgBeginFrame vg 800 600 (exact->inexact (/ 8 6)))
        (nvgBeginPath vg)
        (nvgFontSize vg *font-size*)
        (for ((l *buffer*)
              (i (in-naturals)))
          (renderText vg
                      *padding*
                      (exact->inexact (* (add1 i) *line-height*))
                      l))
        (nvgRect vg
                 (+ *padding* (* *font-size* (current-linepos)))
                 (exact->inexact (+ *padding* (* (current-line) *line-height*)))
                 (/ *font-size* 2)
                 *font-size*)
        (setFill vg 255 255 0 80)
        (nvgFill vg)
        (nvgClosePath vg)
        (nvgEndFrame vg)

        (glfwSwapBuffers win)
        (loop)))))

(glfwTerminate)
