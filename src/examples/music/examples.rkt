#lang s-exp "music-lib.rkt"

(require (prefix-in tr: "../time-racket/time-racket.rkt") (for-syntax rsound racket/function))

(define-art-object (note pitch:id accidental:number octave:number))
(define-art-object (tuning name:id))

(define-art-object (tone freq:number))

(define-converter (note->tone [n : note]) [_ (raise-syntax-error 'note->tone "unimplemented" n)])

;; `!` represents an index into a list in the context (generic)
(define-art-object (! ix:number))

(define-art-object (freq-list item:number ...))
;; this is used to specify the octaves of ! references (multiplies the freqs)
(define-art-object (octave value:number))

(define-converter (note->! [n : note])
  [(_ p:id a:number o:number)
   (define ix
      (match (syntax-e #'p)
       ['c 0] ['d 2] ['e 4] ['f 5] ['g 7] ['a 9] ['b 11]))
   #`(@ [(octave o)] (! #,(+ ix (syntax-e #'a))))])

(define-converter (!->tone [bang : !] {[fl : freq-list] [o : octave]})
  [(_ value:number)
   #`(tone #,(list-ref (cdr (syntax->datum fl)) (syntax-e #'value)))])

(define-converter (tuning->freq-list [t : tuning])
  [(_ {~literal 12tet})
     #'(freq-list 261.63 277.18 293.66 311.13 329.63 349.23 369.99 392.00 415.30 440.00 466.16 493.88)])

(define-converter (note->tone->note->! [nt : note->tone])
  [(_) #'(note->!)])

(define-art sample
  (@ [(score main)]
    (@ [(start 0) (end 14)] (tuning 12tet))
    (@ [(start 0) (end 4) (voice melody)] (note a 0 4))
    (@ [(start 4) (end 8) (voice continuo)] (note c 0 4))))

(run-art sample (execute println))

(define-art converted-sample ;; : !-Music
  sample
  (apply-art (@ [(start 0) (end 100)] (tuning->freq-list) (note->!))))

(run-art converted-sample (execute println))

(define-art tone-sample
  converted-sample
  (apply-art (@ [(start 0) (end 100)] (!->tone))))

(run-art tone-sample (execute println))

(begin-for-syntax
(define (get-score e)
  (context-syntax-ref (get-id-context e) score id))
(define (get-start e)
  (context-value-ref (get-id-context e) start number))
(define (get-end e)
  (context-value-ref (get-id-context e) end number))
(define (get-duration start end)
  (* (/ (- end start) 2) (default-sample-rate))))

(define-art-transform (music->time-racket expr ctxt)
  (syntax-parse expr
    [(_)
     (for/fold ([left (context->list (eval-art-forms (list #'(@ [(tr:start 0) (tr:end 10000000000000)] (define pstream (make-pstream))))))] [right '()] #:result (values (reverse left) right))
               ([stx (context->list ctxt)])
       (syntax-parse stx
         [({~literal tone} fq:number)
          (define start (get-start stx))
          (define end (get-end stx))
          (values (append (context->list (eval-art-forms
            (list #`(@ ([tr:start #,(/ start 2)] [tr:end #,(/ end 2)] [tr:thread a]) (pstream-play pstream (make-tone fq 0.1 #,(get-duration start end)))))))
            left) '())]
         [_ (values left right)]))]))

(define-art the-sounds tone-sample (apply-art (music->time-racket)))
(run-art the-sounds (execute (compose println (curry map un-@) context->list)))
((tr:time-racket->racket the-sounds))

(sleep 14)
