#lang racket/base

(require racket/list)
(require racket/string)
(require racket/bool)
(require racket/serialize)
(require racket/fasl)
(require racket/hash)

(require "trie.rkt")
(require "file-search.rkt")

(provide build-corpus)
(provide save-corpus)
(provide load-corpus)
(provide corpus-get-keys)
(provide create-disk-cached-tree)
(provide lookup-word-sorted)
(provide query-corpus)
(provide query-corpus-multi)
(provide update-datastore-path!)


;; search-tree is implemented as a trie with a hash table as the value of each node
;; each word in the corpus is a key in the trie
;; search-tree also has a hash table of all files in the corpus, with a integer key/index for each file
;; the hash table stored at each node of the trie stores all the hits for that word in the corpus. the table 
;; has file indices for keys and the values are the number of matches for that node's word in the file

(define-serializable-struct search-tree
  ([files #:mutable] ; a hash table mapping integer keys to file names
   [trie #:mutable]  ; the root trie node
   [node-datastore-path #:mutable])) ; false if all the trie's data is stored in memory
  ;#:transparent)

(define (create-search-tree)
  (search-tree (make-hash) (trie-new) #f))

(define (create-new-value file-index)
  (define ht (make-hash))
  (hash-set! ht file-index 1)
  ht)

;; adds a new key to the search tree
(define (insert-new-key key file-index trie)
  (trie-set key 
            (create-new-value file-index)
            trie))

;; key is the word
;; file-index is an index to the search tree's file list
;; increment the number of matches for the file in the hash table stored at this node
;; add a new key to the hash table with 1 match if the file isn't in the hash table already
;; return the trie
(define (update-key-value key file-index value trie)
  (define current-matches (hash-ref value file-index 0))
  (hash-set! value file-index (+ current-matches 1))  
  trie)

(define (insert-word word file-index trie)
  (define key (string->list (string-downcase word)))
  (define value (trie-lookup key trie))
  
  ;(printf "adding word: ~a~n" word)

  (if value
      (update-key-value key file-index value trie)
      (insert-new-key key file-index trie)))

(define (file->word-list path)
  (define (build-list in words)
    (let ([line (read-line in)])
      (cond 
        [(eof-object? line) words]
        [else (build-list in (append (regexp-match* #px"\\b\\w{2,}" line) words))])))

  (call-with-input-file path
    (lambda (in) (build-list in '()))))

;; file-index is an index to the search tree's file list
(define (add-file-to-search-tree path file-index st)
  (define (insert-words words file-index trie)
    (cond
      [(empty? words) trie]
      [else (insert-words (cdr words)
                          file-index
                          (insert-word (car words) file-index trie))]))

  (insert-words (file->word-list path) file-index (search-tree-trie st)))

(define (add-directory-to-search-tree path st)
  (define files (search-tree-files st))
  
  (for ([p (in-directory path)]
        [i 100000])
    ; save the path in the search tree's list of files at index 'i'
    (hash-set! files i p)
    (unless (directory-exists? p) (add-file-to-search-tree p i st))))

(define (read-hash-from-disk path offset)
  ; with-input-from-file won't let me set the file position for some reason
  (define in (open-input-file path))
  (file-position in offset)
  (let ([data (read in)])
    (close-input-port in)
    (deserialize data)))

;; lookup the word in the trie
;; the trie will either return a hash table or a file offset to a hash table
;; convert the hash table into a list of results, with the file index replaced with the file's path 
;; returns a list of (path . num-hits) or '() if no matches
(define (lookup-word st word)
  (define files (search-tree-files st))
  (define cache-path (search-tree-node-datastore-path st))
  (define node (trie-lookup (string->list (string-downcase word)) 
                            (search-tree-trie st)))
  (define ht (cond
               [(not node) #f]
               [cache-path (read-hash-from-disk cache-path node)]
               [else node]))
  (if ht
      (hash-map ht
                (lambda (key value)
                  (cons (hash-ref files key) value)))
      empty))

;; lookup-word returns a list of (key . value)
;; sort on the value
(define (lookup-word-sorted st word)
  (define results (lookup-word st word))
  (if (pair? results)
      (sort results
            #:key cdr
            >)
      empty))

;; matches trie node's subtree as well
;; returns a list of (path . num-hits) or '() if no matches
(define (lookup-word-plus-suffixes st word)
  (define key (string->list (string-downcase word)))
  (define files (search-tree-files st))
  (define cache-path (search-tree-node-datastore-path st))
  (define node (trie-lookup key 
                            (search-tree-trie st)))
  (define ht (cond
               [(not node) #f]
               [cache-path (read-hash-from-disk cache-path node)]
               ;; else copy the hash table so we can do a mutable update to merge the results from suffixes
               [else (hash-copy node)]))

  ;; only match suffixes if the original word is in the trie.
  ;; may want to change this behavior but it doesn't seem right to treat
  ;; a search for "foob" as a search for "foob*" even if "foob" isn't in the corpus.
  (when ht  
    (for ([suffix (trie-get-sub-keys (search-tree-trie st) key #t)])
      (let* ([nd (trie-lookup (append key (string->list suffix))
                              (search-tree-trie st))]
             [node-hash (if (hash? nd) 
                            nd
                            (read-hash-from-disk cache-path nd))])
        (when node-hash 
          (hash-union! ht node-hash #:combine/key (lambda (k v1 v2) (+ v1 v2)))))))
  
  (if ht
      (hash-map ht
                (lambda (key value)
                  (cons (hash-ref files key) value)))
      empty))

;; matches the quoted phrase exactly
;; returns a list of (path . num-hits) or '() if no matches
(define (lookup-phrase st quoted-phrase)
  (define phrase (string-trim quoted-phrase "\""))
  (define query-list (string-split phrase))
  ;; run an AND query on all the words in the phrase to limit number of
  ;; files to search
  (define and-results 
    (and-query st 
               (cdr query-list) 
               (lookup-word st (car query-list))))
  
  (cond [(empty? and-results) empty]
        [(= (length query-list) 1) and-results]
        [else
         (foldl (lambda (potential-file-match file-matches)
                  (let ([num-matches-in-file 
                         (search-file-faster (car potential-file-match) (string->bytes/utf-8 phrase))])
                    (if (= num-matches-in-file 0)
                        file-matches
                        (cons (cons (car potential-file-match)
                                    num-matches-in-file)
                              file-matches))))
                '()
                and-results)])
  )

(define (get-query-func query)
  (if (char=? query #\")
      lookup-phrase
      lookup-word-plus-suffixes))

(define (and-query st query-list results)
  (define (intersect-matches hit intersection)
    (let ([prev-match (assoc (car hit) results)])
      (if prev-match
          ;; set the number of matches equal to the lowest found across all queries
          (if (< (cdr hit) (cdr prev-match))
              (cons hit intersection)
              (cons prev-match intersection))
          intersection)))
  
  (cond 
    [(empty? query-list) results]
    [else 
     (let* ([query-func (get-query-func (string-ref (car query-list) 0))]
            [matches (query-func st (car query-list))])
       (if matches
           (and-query st 
                      (cdr query-list) 
                      (foldl intersect-matches
                             empty
                             matches))
           empty))]
  ))

;; split the query string, treating quoted queries as a single word
(define (query-string-split query-string)
  (define quote-matches (regexp-match* "\".*?\"" query-string))

  (if quote-matches
      (let ([qstring query-string])
        (for ([qmatch quote-matches])
          (set! qstring (string-replace qstring qmatch "")))
        
        (append (string-split qstring) quote-matches))
      (string-split query-string)))

(define (query-corpus st query-string)
  ;; only do AND queries at the moment
  (define query-list (query-string-split query-string))
  ;; get the function to use for the initial query
  (define query-func (get-query-func (string-ref (car query-list) 0)))
  (define results (and-query st 
                             (cdr query-list) 
                             (query-func st (car query-list))))

  ;; probably shouldn't have to check for false here anymore
  (if (or (empty? results) (false? results))
      empty
      (sort results
            #:key cdr
            >)))

;; query multiple corpuses and sort the merged results
(define (query-corpus-multi st-list query-string)
  (for/fold ([results '()]) 
            ([st st-list])
    (sort 
     (append results (query-corpus st query-string))
     >
     #:key cdr)))

(define (build-corpus dir-list)
  (define st (create-search-tree))
  (for ([dir dir-list])
    (add-directory-to-search-tree dir st))
  st)

(define (corpus-get-keys st)
  (trie-get-keys (search-tree-trie st) #t))

(define (save-corpus-fast st path)
  (call-with-output-file path
    (lambda (out) (s-exp->fasl (serialize st) out))))

(define (load-corpus-fast path)
  (call-with-input-file path
    (lambda (in) (deserialize (fasl->s-exp in)))))

(define (save-corpus st path)
  (call-with-output-file path
    (lambda (out) (write (serialize st) out))))

(define (load-corpus path)
  (define st
    (call-with-input-file path
      (lambda (in) (deserialize (read in)))))

  ; Run the garbage collector twice to keep overall memory usage low.
  ; The deserialize will allocate a lot of memory.
  ; Twice is needed because the collector doesn't unmap pages unless
  ; they were unused at the beginning of the previous collection.
  (collect-garbage 'major)
  (collect-garbage 'major)

  (when (and st (search-tree-node-datastore-path st))
    (unless (file-exists? (search-tree-node-datastore-path st))
      ; if the datastore file doesn't exist, perhaps it was moved.
      ; update the datastore path to the same directory as the search tree
      (define-values (base name dir?) (split-path path))
      (define-values (base2 name2 dir2?) (split-path (search-tree-node-datastore-path st)))
      (when (file-exists? (build-path base name2))
        (update-datastore-path! st (build-path base name2)))))
  st)

;; path is the path to the disk cache file to create
;; creates a disk cached trie from an in memory trie
(define (create-disk-cached-tree st path)
  (define mem-trie (search-tree-trie st))
  (define disk-trie (trie-new))

  (call-with-output-file path
    (lambda (out)
      (for ([word (trie-get-keys mem-trie #t)])
        (trie-set (string->list (string-downcase word)) 
                  (file-position out) 
                  disk-trie)
        (write (serialize (trie-lookup (string->list (string-downcase word))
                                       mem-trie)) 
               out))))
  
  ; return a new search tree with a disk cached trie
  (search-tree (hash-copy (search-tree-files st)) 
               disk-trie
               path))

(define (update-datastore-path! st path)
  (if (file-exists? path)
      (set-search-tree-node-datastore-path! st path)
      (error "File path does not exist!")))
