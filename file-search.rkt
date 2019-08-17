#lang racket/base

(require racket/list)
(require racket/port)
(require racket/async-channel)

(provide search-file)
(provide search-file-faster)
(provide search-files-horspool)

;;(define file-list (map (lambda (d) (cons d 0)) (directory-list "/opt/ess/gamefaqs-archive/ps2/final-fantasy-xii/" #:build? #t)))
;;(define file-list (map (lambda (dir) (map (lambda (d) (cons d 0)) (directory-list dir #:build? #t))) (directory-list "/opt/ess/gamefaqs-archive/genesis/" #:build? #t)))
;;(apply append file-list)

;; file search with exact string match on phrase, case sensitive
;; no concept of words, etc.
(define (search-file path phrase)
  (call-with-input-file path
    (lambda (in) 
      (let ([contents (port->string (sync in))]
            [phrase-len (string-length phrase)]
            [phrase-offset 0]
            [hits 0])
        ;(printf "Thread searching file ~a~n" path)
        (for ([ch (in-string contents)])
          (if (= phrase-offset 0)
              (when (char=? ch (string-ref phrase 0))
                (set! phrase-offset (add1 phrase-offset)))
              (if (char=? ch (string-ref phrase phrase-offset))
                  (set! phrase-offset (add1 phrase-offset))
                  (set! phrase-offset 0)))
          (when (= phrase-offset phrase-len)
            (set! hits (add1 hits))
            (set! phrase-offset 0)))
        hits))))

;; takes a path and a byte string
;; avoids creating garbage by reusing a 4k buffer
(define (search-file-faster path phrase-bytes)
  (let ([phrase-len (bytes-length phrase-bytes)]
        [phrase-offset 0]
        [read-buf (make-bytes 4096)]
        ;[total-read 0]
        [hits 0])
    (call-with-input-file path
      (lambda (in)
        (let loop ()
          (let ([num-read (read-bytes! read-buf in 0 4096)])
            (cond 
              [(number? num-read)
               (for ([b (in-bytes read-buf 0 num-read)])  ; need to check num-read in case < 4096 and there is stale data in read-buf
                 (if (= phrase-offset 0)
                     (when (= b (bytes-ref phrase-bytes 0))
                       (set! phrase-offset (add1 phrase-offset)))
                     (if (= b (bytes-ref phrase-bytes phrase-offset))
                         (set! phrase-offset (add1 phrase-offset))
                         (set! phrase-offset 0)))
                 (when (= phrase-offset phrase-len)
                   (set! hits (add1 hits))
                   (set! phrase-offset 0)))
               ; for debugging
               ;(set! total-read (+ total-read num-read))
               ;(printf "total read = ~a, hits = ~a~n" total-read hits)
               (if (= num-read 4096)
                   (loop)
                   hits)]
              [(eof-object? num-read) 
               hits])))))))

;; file-list is a list of (path . num-hits). num-hits can be ignored.
;; phrase is a string
;; num-threads is the number of worker threads to spawn
(define (search-files-with-threads file-list phrase num-threads)
  (define work-channel (make-async-channel num-threads))
  (define main-thread (current-thread))
  (define results '())
  
  (define (send-result data)
    (thread-send main-thread data))
  
  (define (update-results data)
    (set! results (cons data results)))
  
  (define (make-searcher-thread thread-id)
    (thread
     (lambda ()
       (let loop ()
         (define path (async-channel-get work-channel))
         (send-result (cons path (search-file-faster path (string->bytes/utf-8 phrase))))
         (loop)))))
  
  (display "Beginning search...\n")
  
  (for-each make-searcher-thread (range 0 num-threads))
  ;; queue one item for each thread
  (for ([i (in-naturals)]
        [item file-list])
    ;(printf "Searching file ~a: ~a~n" i (car item))
    (if (>= i num-threads)
        (begin 
          (update-results (thread-receive))
          (async-channel-put work-channel (car item)))
        (async-channel-put work-channel (car item))))
  
  ;; if file-list had more items than num-threads, we should still have
  ;; num-threads worth of messages to receive.
  (for ([i num-threads])
    (update-results (thread-receive)))
  
  results)

(define (search-file-regexp path phrase-regex)
  (define in (open-input-file path))
  (define matches (regexp-match* phrase-regex in))
  (close-input-port in)
  matches)

;; Boyer-Moore-Horspool text search implementation

(define (make-skip-table-horspool phrase)
  (define phrase-length (bytes-length (string->bytes/utf-8 phrase)))
  (define skip-table (make-vector 256 phrase-length))
  
  (for ([i (in-range 0 (- phrase-length 1))]
        [ch phrase])
    (vector-set! skip-table (char->integer ch) (- phrase-length i 1)))

  skip-table)

(define (search-file-horspool path needle skip-table)
  (define needle-length (bytes-length needle))
  (define hits 0)
  
  (call-with-input-file path
    (lambda (in) 
      (let* ([buf (port->bytes in)]
             [stop-pos (bytes-length buf)]) 
        (let loop ([cursor (- needle-length 1)])
          (if (>= cursor stop-pos) 
              hits
              (let* ([b (bytes-ref buf cursor)]
                     [skip (vector-ref skip-table b)])
                (cond 
                  [(>= cursor stop-pos) hits]
                  [(not (= b (bytes-ref needle (- needle-length 1)))) 
                   (loop (+ cursor skip))]
                  [else
                   (let loop-match ([i (- cursor 1)]
                                    [needle-index (- needle-length 2)])
                     (cond
                       [(not (= (bytes-ref buf i) 
                                (bytes-ref needle needle-index)))
                        (loop (+ cursor skip))]
                       [(= needle-index 0)
                        (set! hits (add1 hits))
                        (loop (+ cursor skip))]
                       [else
                        (loop-match (- i 1) (- needle-index 1))]))]))))))))


(define (search-files-horspool file-list phrase)
  (define skip-table (make-skip-table-horspool phrase))
  
  (for/list ([item file-list])
    (cons (car item) 
          (search-file-horspool (car item) (string->bytes/utf-8 phrase) skip-table))))
