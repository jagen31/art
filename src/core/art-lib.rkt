#lang racket
(require
  (for-syntax racket "context.rkt" syntax/parse)
  (for-meta 2 syntax/parse))

(begin-for-syntax

;; TODO jagen consider "monomorphizing" (generate unique structs per art language)
(struct art/s [ctxt])
(struct art-transform/s [f])
(struct art-form/s [f])

(define (rewrite-art ctxt)
  (context/s (rewrite-art-exprs (context->list ctxt))))
(define (rewrite-art-in art ctxt)
  (context/s (rewrite-art-exprs-in (context->list art) (context->list ctxt))))
(define (rewrite-art-exprs exprs) (rewrite-art-exprs-in exprs (context/s '())))

(define/contract (rewrite-art-exprs-in to-eval evald)
  (-> context? context? context?)
  (define done-symbol (gensym 'evaluated))
  (define (mark-done expr) (syntax-property expr done-symbol #t))
  (define (rewrite-art-exprs-in to-eval evald)
    (let/ec return
      (when (empty? to-eval) (return evald))
      ;; FIXME jagen improve efficiency
      (define-values (new more) (rewrite-art-expr (car to-eval) (context/s (append (cdr to-eval) evald))))
      (define-values (evald-more unevald-more) (partition (λ(expr) (syntax-property expr done-symbol)) more))
      (define new* (map mark-done (context->list (eval-art-forms new))))
      (rewrite-art-exprs-in unevald-more (append new* evald-more))))
  (context/s (rewrite-art-exprs-in (context->list to-eval) (map mark-done (context->list evald)))))

(define/contract (rewrite-art-expr expr ctxt)
  (-> syntax-with-id-context? context?
      (values (listof syntax-with-id-context?) (listof syntax-with-id-context?)))
  (syntax-parse expr
    [(transform:id args ...)
      #:do [(define transform* (syntax-local-value #'transform))]
      #:when (art-transform/s? transform*)
      ((art-transform/s-f transform*) expr ctxt)]
    [art:id
      (define art* (syntax-local-value #'art))
      (values (context->list (art/s-ctxt art*)) (context->list ctxt))]
    [_ (raise-syntax-error 'rewrite-art
        "can't rewrite expression.  not recognized." expr)]))

(define (eval-art-forms exprs)
  (context/s
    (for/fold ([acc '()])
              ([expr exprs])
      (syntax-parse expr
       [(transform:id args ...)
        #:do [(define transform* (syntax-local-value #'transform (λ() #f)))]
        #:when (art-form/s? transform*)
        (context->list ((art-form/s-f transform*) (add-to-id-context expr (context/s '())) (context/s acc)))]
       [name:id
        #:do [(define art (syntax-local-value #'name (λ() #f)))]
        #:when (art/s? art)
        (append (context->list (art/s-ctxt art)) acc)]
       [_ (cons (add-to-id-context expr (context/s '())) acc)]))))
)

(define-syntax (define-art stx)
  (syntax-parse stx
   [(_ name:id body ...)
    (syntax/loc
      stx
      (define-syntax name
        (let ()
          (define ctxt (eval-art-forms (syntax->list #'(body ...))))
          (art/s ctxt))))]))

(define-syntax (run-art stx)
  (syntax-parse stx
   [(_ body ...)
    (syntax/loc
      stx
      (begin-for-syntax
        (eval-art-forms (syntax->list #'(body ...)))
        (void)))]))

(define-syntax (define-art-object stx)
  (syntax-parse stx
    [(_ (name:id args ...))
     (syntax/loc stx
       (define-art-transform (name expr ctxt)
         (syntax-parse expr
           [({~literal name} args ...) (void)]
           [_ (raise-syntax-error 'name "invalid syntax" expr)])
         (values (list expr) (context->list ctxt))))]))

(define-syntax (define-art-form stx)
  (syntax-parse stx
    ;; FIXME jagen make these parameters or something?
    [(_ (name:id expr-id:id ctxt-id:id) body ...)
     (syntax/loc stx
       (define-syntax name (art-form/s (λ(expr-id ctxt-id) body ...))))]))

(define-syntax (define-art-transform stx)
  (syntax-parse stx
    ;; FIXME jagen make these parameters or something?
    [(_ (name:id expr-id:id ctxt-id:id) body ...)
     (syntax/loc stx
       (define-syntax name (art-transform/s (λ(expr-id ctxt-id) body ...))))]))

;; FIXME use eval-art-forms for this recursion
(define-art-form (@ expr ctxt)
  (define (go stx ctxt)
    (syntax-parse stx
      [({~literal @} (expr ...) body ...)
       (define id-ctxt (get-id-context stx))
       (define id-ctxt* (basic-merge-ctxt (eval-art-forms (syntax->list #'(expr ...))) id-ctxt))
       (apply append (map (λ(x) (go (add-to-id-context x id-ctxt*) ctxt)) (syntax->list #'(body ...))))]
      [e (list this-syntax)]))
  (context/s (append (go expr ctxt) (context->list ctxt))))

(define-for-syntax (un-@ expr)
  (define id-ctxt (get-id-context expr))
  (quasisyntax/loc expr (@ [#,@(context->list id-ctxt)] #,expr)))

(define-art-form (apply-art expr ctxt)
  (syntax-parse expr
    [(_ body ...)
     (rewrite-art-exprs-in (eval-art-forms (syntax->list #'(body ...))) ctxt)]))

(define-art-form (execute expr ctxt)
  (syntax-parse expr
    [(_ f) ((eval-syntax #'f) ctxt) (context/s '())]))

(provide run-art define-art define-art-form define-art-transform
         define-art-object
         execute
         @ apply-art
         (for-syntax rewrite-art-exprs rewrite-art-exprs-in eval-art-forms un-@ (all-from-out "context.rkt")))
