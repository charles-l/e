(require parsack)
(require (only-in srfi/1 any))
(require sugar/list)

(define *types* '("char" "uchar" "short" "ushort" "int" "uint" "float" "double"))

(define $comment (>> (string "comment")
                     (manyUntil $anyChar $newline)))

(define $word (>>= (manyUntil (noneOf " \n\t\r") $space)
                   (compose return list->string)))

;; WARNING: this function will recurse if no $space is consumed - make sure to consume the whitespace to advance
(define $strict-word (>>= (manyUntil $anyChar (lookAhead $space))
                          (compose return list->string)))

(define $line (manyUntil
                (parser-one
                  (~> $strict-word) (<or> (char #\space) (return null))) $newline))

(parse-result $strict-word "asdf asdf asdf as\nasdf")


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
    (~ (word "element"))
    (>>= $word return-sym)
    $number
    (many1 $property)))

(define $header
  (parser-compose (string "ply") $newline
                  (string "format ascii 1.0") $newline
                  (many1Until (<or> $element
                                    (>> $comment (return null)))
                              (try (word "end_header")))))



(define element-name car)
(define element-count cadr)
(define element-property-list caddr)

(define (body header)
  (for/vector #:length (length header) ((e header))
    (for/vector #:length (element-count e) ((i (in-range (element-count e))))
      (parse-result $line (current-input-port)))))



(define (parse-ply filename)
  (with-input-from-file filename
                        (λ ()
                           (let* ((res (parse-result $header (current-input-port)))
                                  (header (filter-not null? res)))
                             (cons header
                                   (body header))))))

