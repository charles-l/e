#lang racket
(require ffi/unsafe
         ffi/unsafe/define)

(define-ffi-definer define-native (ffi-lib "shared"))

(define _WINDOW-ptr (_cpointer 'GLFWwindow))

(define-native init (_fun -> _WINDOW-ptr))
(define-native compileTextShader (_fun -> _uint))
(define-native moveCamera (_fun _uint _float _float -> _void))
(define-native glfwWindowShouldClose (_fun _WINDOW-ptr -> _int))
(define-native glfwPollEvents (_fun -> _void))
(define-native prepareRendering (_fun _uint _float _float _float -> _void))
(define-native renderChar (_fun _uint8 _float _float _float -> _int))
(define-native glfwSwapBuffers (_fun _WINDOW-ptr -> _void))
(define-native glfwTerminate (_fun -> _void))

(define-native glfwSetCharCallback (_fun _WINDOW-ptr (_fun _WINDOW-ptr _uint -> _void) -> _void))
(define-native glfwSetKeyCallback (_fun _WINDOW-ptr (_fun _WINDOW-ptr _int _int _int _int -> _void) -> _void))

(define *buffer* (vector ""))

(define glfw-key-enter 257)
(define glfw-press 1)

(define (key-handler win key scancode action mods)
  (when (and (eq? key glfw-key-enter) (eq? action glfw-press))
    (set! *buffer* (vector-append *buffer* #("")))))

(define (vector-last-i v)
  (- (vector-length v) 1))

(define (char-handler win codept)
  (vector-set!
    *buffer*
    (vector-last-i *buffer*)
    (string-append (vector-ref *buffer* (vector-last-i *buffer*)) (~a (integer->char codept))))
  (print (integer->char codept))
  (newline))

(define (render-string s y)
  (for/list ((e (map char->integer (string->list s)))
             (i (in-range 0 (string-length s))))
    (renderChar e (* (exact->inexact i) 24.0) (- 550.0 (* y 40)) 1.0)))

(let* ((win (init))
       (shader-id (compileTextShader)))
  (glfwSetCharCallback win char-handler)
  (glfwSetKeyCallback win key-handler)
  (moveCamera shader-id 0.0 0.0)
  (let loop ()
    (when (zero? (glfwWindowShouldClose win))
      (glfwPollEvents)
      (prepareRendering shader-id 0.0 1.0 1.0)
      (for ((i (in-range 0 (vector-length *buffer*))))
        (render-string (vector-ref *buffer* i) i))
      (glfwSwapBuffers win)
      (loop))))

(glfwTerminate)
