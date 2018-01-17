#lang racket
(provide window-class%
         glfw-key-enter
         glfw-key-backspace
         glfw-press
         *padding*
         *line-height*
         *font-size*)
(require ffi/unsafe
         ffi/unsafe/define)

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
(define-native calcCharXPos (_fun _nvg-ptr _string _int -> _float))
(define-native renderText (_fun _nvg-ptr _float _float _string -> _void))
(define-native clear (_fun -> _void))

(define-native glfwSetCharCallback (_fun _WINDOW-ptr (_fun _WINDOW-ptr _uint -> _void) -> _void))
(define-native glfwSetKeyCallback (_fun _WINDOW-ptr (_fun _WINDOW-ptr _int _int _int _int -> _void) -> _void))

(define glfw-key-enter 257)
(define glfw-key-backspace 259)
(define glfw-press 1)

(define *font-size* 15.0)
(define *line-height* 15.0)
(define *padding* 5.0)

(define window-class%
  (class object%
    (init-field char-handler key-handler)

    (define %%window (init))
    (define %%nvg-context (initVG %%window))

    (super-new)

    (glfwSetCharCallback %%window char-handler)
    (glfwSetKeyCallback %%window key-handler)

    (define/public (calculate-character-x str i)
      (calcCharXPos %%nvg-context str i))

    (define/public (draw-text x y t)
      (renderText %%nvg-context x y t))

    (define/public (set-color r g b a)
      (setFill %%nvg-context r g b a))

    (define/public (draw-rect x y w h)
      (nvgBeginPath %%nvg-context)
      (nvgRect %%nvg-context x y w h)
      (nvgFill %%nvg-context))

    (define/public (render-loop thunk)
      (let loop ()
        (unless (glfwWindowShouldClose %%window)
          (glfwPollEvents)
          (clear)
          (nvgBeginFrame %%nvg-context 800 600 (exact->inexact (/ 8 6)))
          (nvgFontSize %%nvg-context *font-size*)
          (thunk)
          (nvgEndFrame %%nvg-context)
          (glfwSwapBuffers %%window)
          (loop)))
      ; TODO: cleanup nvg
      (glfwTerminate))))
