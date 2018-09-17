#lang sicp

;;; copy the following forms in the REPL for ambiguous evaluator

(define (not x)
  (if x false true))

(define (require p)
  (if (not p) (amb)))

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

(define (gen-word word-list)
  (define (gen word-list)
    (require (not (null? word-list)))
    (ramb (list (car word-list))
          (gen (cdr word-list))))
  (gen (cdr word-list)))

(define (gen-simple-noun-phrase)
  (append (gen-word articles)
          (gen-word nouns)))

(define (gen-noun-phrase)
  (define (maybe-extend noun-phrase)
    (ramb noun-phrase
         (append noun-phrase
                 (gen-prepositional-phrase))))
  (maybe-extend (gen-simple-noun-phrase)))

(define (gen-prepositional-phrase)
  (append (gen-word prepositions)
          (gen-noun-phrase)))

(define (gen-verb-phrase)
  (define (maybe-extend verb-phrase)
    (ramb verb-phrase
          (append verb-phrase
                  (gen-prepositional-phrase))))
  (maybe-extend (gen-word verbs)))

(define (gen-sentence)
  (append (gen-noun-phrase)
          (gen-verb-phrase)))
