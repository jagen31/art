#lang racket

(require "../../core/art-lib.rkt" "sketchy.rkt"
  syntax/parse (for-syntax syntax/parse) racket/stxparam)

(define picture-sketchy
  (make-sketchy
    #'(subcontext canvas
        (plane
          (number-line xstart xend 0 400 100)
          (number-line ystart yend 0 400 100)))))

(define-art picture
  (@ [(canvas main)]
    (in [(xstart 0) (xend 300) (ystart 0) (yend 300)] (color red))
    (in [(xstart 0) (xend 150) (ystart 0) (yend 150)] (circle))
    (in [(xstart 150) (xend 300) (ystart 150) (yend 300)] (rectangle))))

(picture-sketchy 300 300 picture)
