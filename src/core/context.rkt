#lang racket
(require syntax/parse racket/match syntax/id-table (for-syntax syntax/parse) )


(provide

  ;; context
  (contract-out
    [context/s (-> (listof syntax?) context?)]
    [context->list (-> context? (listof syntax?))]
    [context? (-> any/c boolean?)]
    [context-ref (-> context? identifier? (or/c syntax? #f))]
    [partition-context (-> context? identifier? immutable-free-id-table?)]
    [basic-merge-ctxt (-> context? context? context?)])
  context-syntax-ref
  context-value-ref
  no-enum-id

  ;; contexter
  define-contexter
  contexter/c
  (contract-out
    [compose-contexters (-> contexter/c contexter/c contexter/c)]
    [run-contexter (-> contexter/c context? context?)])
  type-contexter

  ;; id context
  (contract-out
    [add-to-id-context (-> syntax? context? syntax-with-id-context?)]
    [get-id-context (-> syntax? (or/c context? #f))]
    [copy-id-context (-> syntax-with-id-context? syntax? syntax-with-id-context?)]
    [syntax-with-id-context? (-> any/c boolean?)]))

;; FIXME jagen bad idea
(define no-enum-id #'none)

(struct context/s [stxs] #:transparent)
(define context->list context/s-stxs)
(define (context? ctxt)
  (and (context/s? ctxt) ((listof syntax?) (context/s-stxs ctxt))))

(define contexter/c (-> context? (values context? (listof syntax?))))

(define-syntax (define-contexter stx)
  (syntax-parse stx
    [(_ (name [arg arg-contract] ...) filter-fun)
     (syntax/loc stx
       (define/contract ((name arg ...) ctxt)
         (-> arg-contract ... contexter/c)
         (define-values (inner outer) (partition filter-fun (context->list ctxt)))
         (values (context/s inner) outer)))]))


(define-contexter (type-contexter [id identifier?])
  (syntax-parser [(head more ...) (and (free-identifier=? #'head id) (syntax-e this-syntax))]))

(define ((compose-contexters left right) ctxt)
  (-> contexter/c contexter/c contexter/c)
  (define-values (inner-r outer-r) (right ctxt))
  (define-values (inner outer-l) (left inner-r))
  (values inner (append outer-l outer-r)))

(define (run-contexter contexter ctxt)
  (define-values (inner _) (contexter ctxt))
  inner)

(define (context-ref ctxt id)
  (define-values (ctxt* _) ((type-contexter id) ctxt))
  (define ctxt** (context->list ctxt*))
  (and (not (empty? ctxt**)) (car ctxt**)))

(define-syntax (context-syntax-ref stx)
  (syntax-parse stx
    [(_ ctxt the-id:id clazz:id)
     (syntax/loc stx
       (let ()
         (define blah (context-ref ctxt #'the-id))
         (syntax-parse blah
           [(_ {~var value clazz}) #'value]
           [_ #f])))]
    [_ (error 'oops "oops")]))

(define-syntax (context-value-ref stx)
  (syntax-parse stx
    [(_ args ...) #`(syntax-e (or (context-syntax-ref args ...) #'#f))]
    [_ (error 'oops "oops")]))

(define (partition-context ctxt prop)
  (for/fold ([acc (make-immutable-free-id-table)])
            ([stx (context->list ctxt)])
    (define ctxt* (context-ref (get-id-context stx) prop))
    (define ctxt
      (syntax-parse ctxt*
        [(_ value:id) #'value]
        [_ no-enum-id]))
    (dict-update acc ctxt (λ(li) (cons stx li)) '())))

(define (basic-merge-ctxt c1 c2)
  (match* (c1 c2)
    [((context/s l1) (context/s l2)) (context/s (append l1 l2))]))


(define id-context-symbol (gensym))

(define (syntax-with-id-context? val)
  (and (syntax? val) (context? (syntax-property val id-context-symbol))))

;; identity context is embedded as a syntax property.
(define (add-to-id-context stx props)
  (define existing (syntax-property stx id-context-symbol))
  (syntax-property stx id-context-symbol
    (if existing (basic-merge-ctxt props existing) props)))

(define (get-id-context stx)
  (syntax-property stx id-context-symbol))

(define (copy-id-context og new)
  (syntax-property new id-context-symbol (syntax-property og id-context-symbol)))
