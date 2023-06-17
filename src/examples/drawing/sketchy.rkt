#lang racket
(require 2htdp/image syntax/parse "../../core/context.rkt" syntax/id-table)
(provide (all-from-out 2htdp/image))


;; FIXME jagen make this a type instead of curried function?
(define ((make-sketchy coord-stx) width height ctxt)
  (syntax-parse coord-stx
    [({~literal subcontext} name:id subexpr)
     (define inner-sketchy (make-sketchy #'subexpr))

     (define subcontexts
       (for/fold ([acc (make-immutable-free-id-table)])
                 ([stx (context->list ctxt)])
         (define ctxt* (context-ref (get-id-context stx) #'name))
         (define ctxt
           (syntax-parse ctxt*
             [(_ value:id) #'value]
             [_ no-enum-id]))
         (dict-update acc ctxt (λ(li) (cons stx li)) '())))

     (for/fold ([acc empty-image])
               ([(name ctxt) (in-dict subcontexts)])
       (above/align 'left
                    acc
                    (overlay/align 'left 'top (text (~s (syntax->datum name)) 32 'black)
                                              (rectangle width 40 'solid 'gray))
                    (inner-sketchy width (/ height (length (dict-keys subcontexts))) (context/s ctxt))))]
    [({~literal plane} x-expr y-expr)
     ((make-sketchy-plane #'x-expr #'y-expr) width height ctxt)]))

(define ((make-sketchy-plane x-expr y-expr) width height ctxt)
  (define x-axis ((make-sketchy-axis width 40 x-expr) ctxt))
  (define y-axis ((make-sketchy-axis height 40 y-expr) ctxt))
  (define music-graph
    (for/fold ([acc (rectangle width height 'solid 'white)])
              ([stx (context->list ctxt)] [i (in-naturals)])
      (define-values (x-start x-end) ((axis/s-ruler x-axis) stx))
      (define-values (y-start y-end) ((axis/s-ruler y-axis) stx))
      (define-art result
        (@ [(x-start x-start) (x-end x-end) (y-start y-start) (y-end y-end)]
          (@ [(color 'purple)] (text stx 24) )
      (overlay/xy
          (overlay/xy (text (~s (syntax->datum stx)) 24 'black)
            0 (- (* i (/ (- y-end y-start) (length (context->list ctxt)))))
            (rectangle (- x-end x-start) (- y-end y-start) 'outline
              (pen (list-ref '(blue purple green) (modulo i 3)) 5 'dot-dash 'round 'round)))
          (- x-start) (- y-start)
          acc)))
      (beside/align 'top
        (rotate -90 (axis/s-picture y-axis))
        (above/align 'left music-graph (axis/s-picture x-axis))))

(struct axis/s [ruler picture] #:transparent)

(define ((make-sketchy-axis width height stx) ctxt)
  (syntax-parse stx
    #:datum-literals [enum number-line]
    [(number-line x-prop y-prop start end interval)
     (define max-end (syntax-e #'end))
     (make-number-line width height #'x-prop #'y-prop
                       (syntax-e #'start) max-end (syntax-e #'interval))]
   [(enum prop:id [val ...])
     (define vals (syntax->list #'(val ...)))
     (make-enum width height #'prop vals)]))

(define (make-number-line width height start-stx end-stx min-start max-end interval)

  (define (make-number num) (above/align 'left (line 0 10 'black) (text (~s num) 24 'black)))

  (define beat-width (/ width (/ (- max-end min-start) interval)))

  (define (ruler stx)
    (define start (context-ref (get-id-context stx) start-stx))
    (define end (context-ref (get-id-context stx) end-stx))
    (define start*
      (syntax-parse start
        [(_ value:number) (syntax-e #'value)]
        [_ min-start]))
    (define end*
      (syntax-parse end
        [(_ value:number) (syntax-e #'value)]
        [_ max-end]))
    (values (* beat-width (/ start* interval)) (* beat-width (/ end* interval))))

  (define picture
    (underlay/align
      'left 'top
      (rectangle width height 'solid 'gray)
      (for/fold ([acc empty-image])
                ([num (in-range min-start max-end interval)])
        (beside acc
          (overlay/align 'left 'top (make-number num)
            (rectangle beat-width height 'solid 'transparent))))))

  (axis/s ruler picture))

(define (make-enum width height prop vals)

  (define vals* (cons no-enum-id vals))

  (define (draw-value val) (text (~s (syntax->datum val)) 24 'black))

  (define val-width (/ width (length vals*)))

  (define (ruler stx)
    (define val (context-ref (get-id-context stx) prop))
    (define val*
      (syntax-parse val
        [(_ value:id) #'value]
        [_ #'none]))
    (define ix (index-where vals* (λ(s) (free-identifier=? val* s))))
    (values (* val-width ix) (* (add1 ix) val-width)))

  (define picture
    (underlay
      (rectangle width 40 'solid 'gray)
      (for/fold ([acc empty-image])
                ([val* vals*])
        (beside acc
          (overlay (draw-value val*) (rectangle val-width height 'solid 'transparent))))))

  (axis/s ruler picture))

(provide make-sketchy no-enum-id)
