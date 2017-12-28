(require parsack)
(require (only-in srfi/1 any))
(require sugar/list)

(define *types* '("char" "uchar" "short" "ushort" "int" "uint" "float" "double"))

(define $comment (parser-compose
                   (string "comment")
                   (manyUntil $anyChar $newline)
                   (return null)))

(define $word (>>= (manyUntil (noneOf " \n\t\r") $space)
                   (compose return list->string)))

(define (word s)
  (parser-compose
    (s <- (string s))
    $space
    ((compose return list->string) s)))

(define return-sym (compose return string->symbol))

(define $number (>>= (many1Until $digit $space)
                     (compose return string->number list->string)))

(define $type
  (<or>
    (parser-seq
      (~ (word "list")) $type $type)
    (parser-compose
      (choice (map (compose try word) *types*)))))

(define $property
  (parser-seq
    (~ (word "property"))
    $type
    (>>= $word return-sym)))

(define $element
  (parser-seq
    (>>= (word "element") return-sym)
    (>>= $word return-sym)
    $number
    (many1 $property)))

(define $header
  (parser-compose (string "ply") $newline
                  (string "format ascii 1.0") $newline
                  (many1Until (<or> $element
                                    $comment)
                              (try (word "end_header")))))



(define element-name car)
(define element-count cadr)
(define element-property-list caddr)

; generates a list of indexes of requested properties from the header
; expects desired fields in the format: ((element-name property ...) ...)
(define (mask header desired-fields)
  (map
    (lambda (desired-element)
      (let ((properties
              (element-property-list
                (assoc (car desired-element) header))))
        (let ((r (map
                   (lambda (x)
                     (index-of
                       properties
                       x
                       (lambda (a b)
                         (eq? (cadr a) b))))
                   (cdr desired-element))))
          (if (any false? r)
            (error "unexpected property in" desired-fields)
            (cons (car desired-element) r)))))
    desired-fields))


(define (body header desired-fields)
  (for ((e header))
    (for ((i (in-range (cadr e))))
      (parse-result
        (make-list (length (caddr e)) $word) (current-input-port)))))

; TODO: select desired fields when parsing body

(with-input-from-file "x.ply"
                      (λ ()
                         (let* ((res (parse-result $header (current-input-port)))
                                (header (map cdr (filter-not null? res))))
                           header)))
