#lang racket
(provide window-class%
         glfw-key-left
         glfw-key-right
         glfw-key-up
         glfw-key-down
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

(define _ctx-ptr (_cpointer 'context))
(define _win-ptr (_cpointer 'window))
(define _nvg-ptr (_cpointer 'vg))

(define-native init (_fun -> _ctx-ptr))
(define-native cleanup (_fun _ctx-ptr -> _void))
(define-native getWindow (_fun _ctx-ptr -> _win-ptr))
(define-native getVGContext (_fun _ctx-ptr -> _nvg-ptr))
(define-native shouldQuit (_fun _ctx-ptr -> _bool))
(define-native clear (_fun -> _void))
(define-native startFrame (_fun _ctx-ptr -> _void))
(define-native endFrame (_fun _ctx-ptr -> _void))
(define-native setFill (_fun _ctx-ptr _uint8 _uint8 _uint8 _uint8 -> _void))
(define-native setStrokeColor (_fun _ctx-ptr _uint8 _uint8 _uint8 _uint8 -> _void))
(define-native renderText (_fun _ctx-ptr _float _float _string -> _void))
(define-native drawRect (_fun _ctx-ptr _float _float _float _float -> _void))
(define-native drawLine (_fun _ctx-ptr _float _float _float _float -> _void))
(define-native calcCharXPos (_fun _ctx-ptr _string _int -> _float))

(define-native nvgMoveTo (_fun _nvg-ptr _float _float -> _void))
(define-native nvgEndFrame (_fun _nvg-ptr -> _void))
(define-native nvgBeginPath (_fun _nvg-ptr -> _void))
(define-native nvgClosePath (_fun _nvg-ptr -> _void))
(define-native nvgRect (_fun _nvg-ptr _float _float _float _float -> _void))
(define-native nvgFill (_fun _nvg-ptr -> _void))
(define-native nvgFontSize (_fun _nvg-ptr _float -> _void))
(define-native nvgTextLetterSpacing (_fun _nvg-ptr _float -> _void))

(define-native glfwSetCharCallback (_fun _win-ptr (_fun _win-ptr _uint -> _void) -> _void))
(define-native glfwSetKeyCallback (_fun _win-ptr (_fun _win-ptr _int _int _int _int -> _void) -> _void))

(define glfw-key-enter 257)
(define glfw-key-backspace 259)
(define glfw-key-right 262)
(define glfw-key-left 263)
(define glfw-key-down 264)
(define glfw-key-up 265)
(define glfw-press 1)

(define *font-size* 15.0)
(define *line-height* 15.0)
(define *padding* 5.0)

(define window-class%
  (class object%
    (init-field char-handler key-handler)

    (define %%ctx (init))

    (super-new)

    (glfwSetCharCallback (getWindow %%ctx) char-handler)
    (glfwSetKeyCallback (getWindow %%ctx) key-handler)

    (define/public (calculate-character-x str i)
      (calcCharXPos %%ctx str i)
      )

    (define/public (text x y t)
      (renderText %%ctx x y t))

    (define/public (set-color r g b a)
      (setFill %%ctx r g b a))

    (define/public (set-stroke-color r g b a)
      (setStrokeColor %%ctx r g b a))

    (define/public (rect x y w h)
      (drawRect %%ctx x y w h))

    (define/public (line x1 y1 x2 y2)
      (drawLine %%ctx x1 y1 x2 y2))

    (define/public (render-loop thunk)
      (let loop ()
        (unless (shouldQuit %%ctx)
          (startFrame %%ctx)
          (nvgFontSize (getVGContext %%ctx) *font-size*)
          (thunk)
          (endFrame %%ctx)
          (loop)))
      (cleanup %%ctx))))
