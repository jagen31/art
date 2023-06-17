#lang racket
(require "../../core/context.rkt" "../../core/art-lib.rkt" "../drawing/sketchy.rkt" (prefix-in rack: racket)
  syntax/parse (for-syntax racket syntax/parse "../drawing/sketchy.rkt" "../../core/context.rkt" "../../core/art-lib.rkt") racket/stxparam)

(define-syntax-parameter thread-name (λ(stx) (raise-syntax-error 'oops stx)))

(define-art-object (thread name:id))
(define-art-object (start value:number))
(define-art-object (end value:number))
(define-art-object (expr _))

(begin-for-syntax
(define (compile-time-racket ctxt)
  (define by-thread (partition-context ctxt #'thread))
  (define (process name ctxt)
    (define (start-thread-stx body)
      #`(rack:thread (λ() (parameterize ([thread-name/p '#,name]) #,body))))
    (start-thread-stx (compile-time-racket-exprs ctxt)))

  (define (init bodies)
    #`(syntax-parameterize ([thread-name (syntax-parser [_ #'(thread-name/p)])])
        #,@(dict-ref by-thread no-enum-id) #,@bodies))

  (println (dict-keys by-thread))

  (define by-thread* (dict-remove by-thread no-enum-id))

  (define result
  #`(lambda ()
     (define thread-name/p (make-parameter #f))
     #,(init (for/fold ([acc '()] #:result (reverse acc))
                       ([(name ctxt) (in-dict by-thread*)])
               (cons (process name ctxt) acc)))))
  result)

(define (compile-time-racket-exprs exprs)
  (define (get-start e)
    (context-value-ref (get-id-context e) start number))

  (println "ehreherh")
  (println exprs)
  (define sorted-exprs (sort exprs (λ(l r) (if l (if r (< l r) #f) #t)) #:key get-start))
  (define (compile-racket-exprs exprs t)
    (let/ec return
      (when (empty? exprs) (return '()))
      (define start (get-start (car exprs)))
      (println "HERE")
      (println start)
      (when (or (not start) (= t start))
        (return (cons (car exprs) (compile-racket-exprs (rest exprs) start))))
      (return (cons #`(sleep #,(- start t)) (compile-racket-exprs exprs start)))))
  #`(begin #,@(compile-racket-exprs sorted-exprs 0)))
)

(define time-racket-sketchy
  (make-sketchy
    #'(plane
        (number-line start end 0 30 2)
        (enum thread [a b c]))))

(define-syntax (time-racket->racket stx)
  (syntax-parse stx
    [(_ body ...) (compile-time-racket (eval-art-forms (syntax->list #'(body ...))))]))

(provide time-racket->racket thread start end expr)

(module+ test
  (define-art sample
    (@ []
      (define x (void))
      (define (logf msg . vars)
        (apply printf (string-append
                      "[thread " (~s thread-name) "] " msg) vars))
      (@ [(thread a) (start 0) (end 4)]
        (begin (set! x 41) (logf "hello, x is now: ~s\n" x)))
      (@ [(thread b) (start 4) (end 8)]
        (begin (logf "adding 1 to x!\n") (set! x (add1 x))))
      (@ [(thread c) (start 8) (end 12)] (logf "x is now: ~s\n" x))))

  ((time-racket->racket sample))
  (sleep 14))
