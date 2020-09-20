#lang racket/base

(require racket/list)
(require racket/serialize)

(provide trie-new)
(provide trie-lookup)
(provide trie-set)
(provide trie-get-keys)
(provide trie-get-sub-keys)

(struct trie-node
  ([value #:mutable]
   [links #:mutable]) ; assoc list of trie-nodes
  #:prefab)

(define (trie-new)
  (trie-node #f '()))

(define (follow-link key extend? trie)
  (define link (assoc key (trie-node-links trie)))
  (cond 
    [link (cdr link)]
    [extend?
     (define new-link (cons key (trie-new)))
     (set-trie-node-links! trie (cons new-link (trie-node-links trie)))
     (cdr new-link)]
    [else #f]))

(define (lookup key extend? trie)
  (cond
    ;[(empty? key) (trie-node-value trie)]
    [(empty? key) trie]
    [else
     (define next-link (follow-link (car key) extend? trie))
     (if next-link
         (lookup (cdr key) extend? next-link)
         #f)]))

(define (trie-lookup key trie)
  (define node (lookup key #f trie))
  (if node 
      (trie-node-value node)
      #f))

(define (trie-set key value trie)
  (define node (lookup key #t trie))
  (set-trie-node-value! node value)
  trie)

(define (map2 f arg2 lst)
  (cond 
    [(empty? lst) empty]
    [else (cons (f (car lst) arg2)
                (map2 f arg2 (cdr lst)))]))

(define (trie-get-keys trie key-as-string?)
  (define (get-keys tlink prev-key)
    (define key (if key-as-string?
                    (string-append prev-key (string (car tlink)))
                    (append prev-key (cons (car tlink) '()))))
    (define trie (cdr tlink))
    (cond
      ; assume leaf nodes always have a value
      [(empty? (trie-node-links trie)) key]
      [(trie-node-value trie) (cons key (map2 get-keys key (trie-node-links trie)))]
      [else (map2 get-keys key (trie-node-links trie))]))

  (if key-as-string?
      (flatten (map2 get-keys "" (trie-node-links trie)))
      (map2 get-keys empty (trie-node-links trie))))

(define (trie-get-sub-keys trie key key-as-string?)
  (define node (lookup key #f trie))
  (if node
      (trie-get-keys node key-as-string?)
      '()))
