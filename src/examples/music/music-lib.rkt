#lang racket
(require "../../core/art-lib.rkt"
         (for-syntax racket "../../core/context.rkt" syntax/parse "../drawing/sketchy.rkt")
         (for-meta 2 syntax/parse)
         rsound)

(define (get-duration start end)
  (* (/ (- end start) 2) (default-sample-rate)))

(begin-for-syntax

(define music-sketchy
  (make-sketchy
    #'(subcontext score
        (plane
          (number-line start end 0 20 2)
          (enum voice [melody continuo])))))

(define (get-score e)
  (context-syntax-ref (get-id-context e) score id))
(define (get-start e)
  (context-value-ref (get-id-context e) start number))
(define (get-end e)
  (context-value-ref (get-id-context e) end number))
(define (compile-music* ctxt) (compile-music-exprs (context->list ctxt)))

(provide compile-music*)

(define (compile-music-exprs full-exprs)

  (define full-ctxt (context/s full-exprs))
  (define (compile-music-exprs exprs t)
    (let/ec return
      (when (empty? exprs) (return #'(silence 1)))
      (define expr (car exprs))
      (define start (get-start expr))
      (define sound
        (if (= start 0) #'expr #`(rs-append (silence (get-duration 0 start)) expr)))
      (return #`(rs-overlay #,sound #,(compile-music-exprs (cdr exprs) start)))))
  (compile-music-exprs full-exprs 0))

(define-contexter (outer-time-contexter [start number?] [end number?])
  (λ(stx)
    (define id-ctxt (get-id-context stx))
    (define start* (context-value-ref id-ctxt start number))
    (define end* (context-value-ref id-ctxt end number))
    (and (>= start start*) (<= end end*))))

(define-contexter (inner-time-contexter [start number?] [end number?])
  (λ(stx)
    (define id-ctxt (get-id-context stx))
    (define start* (context-value-ref id-ctxt start number))
    (define end* (context-value-ref id-ctxt end number))
    (and (>= start* start) (<= end* end))))

(define-contexter (score-contexter [name identifier?])
  (λ(stx)
    (define id-ctxt (get-id-context stx))
    (free-identifier=? name (context-syntax-ref id-ctxt score id))))

(provide outer-time-contexter inner-time-contexter score-contexter)
)

;; music forms go at the top level of music and are evaluated immediately
;; music transforms are objects with an attached metafunction that the music form `rewrite` will apply

;; locations for music
(define-art-object (score name:id))
(define-art-object (start value:number))
(define-art-object (end value:number))
(define-art-object (voice name:id))

;; shorthand for a converter from one type to another- basically, a `map` over exprs in a context.
;; TODO jagen abstract into art-lib
(define-syntax (define-converter stx)
  (syntax-parse stx
    [(_ (name:id [arg:id {~literal :} typ:id]) body ...)
     #'(define-converter (name [arg : typ] {}) body ...)]
    [(_ (name:id [arg:id {~literal :} typ:id] {[ctxt-arg:id {~literal :} ctxt-typ:id] ...}) body ...)
     #:with parse #'(syntax-parse arg #:datum-literals [type] body ...)
     (quasisyntax/loc stx
       (begin
         (define-art-transform (name expr ctxt)
           ;; FIXME jagen validate expr
           (define (go arg)
             (define inner-start (get-start arg))
             (define inner-end (get-end arg))
             ;; FIXME jagen figure out scopes.  also figure out merging context with id context.
             (define outer-ctxt (run-contexter (outer-time-contexter inner-start inner-end) ctxt))
             (define outer-ctxt* (basic-merge-ctxt outer-ctxt (get-id-context arg)))
             (define ctxt-arg (car (context->list (run-contexter (type-contexter #'ctxt-typ) outer-ctxt*)))) ...
             (copy-id-context arg (datum->syntax arg (syntax->datum parse))))
           (define start (get-start expr))
           (define end (get-end expr))
           (define-values (objects rest)
             ((compose-contexters (type-contexter #'typ) (inner-time-contexter start end)) ctxt))
           (values (map go (context->list objects)) rest))))]))

(provide define-converter
         score start end voice
         (all-from-out racket rsound "../../core/art-lib.rkt")
         (for-syntax (all-from-out racket syntax/parse)))
