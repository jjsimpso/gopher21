#lang racket/base

(require racket/list)
(require racket/string)
(require racket/bool)
(require racket/serialize)
(require racket/fasl)
(require racket/hash)
(require racket/set)
(require racket/path)

(require "trie.rkt")
(require "file-search.rkt")

(provide build-corpus)
(provide save-corpus)
(provide load-corpus)
(provide corpus-get-keys)
(provide create-disk-cached-tree)
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
   [node-datastore-path #:mutable] ; false if all the trie's data is stored in memory
   [root-path #:mutable])) ; root directory where text is located
  ;#:transparent)

(define (create-search-tree)
  (search-tree (make-hash) (trie-new) #f (string->path "/")))

(define (create-new-value file-index)
  (define ht (make-hash))
  (hash-set! ht file-index 1)
  ht)

;; takes a file index and returns the absolute path to the file
(define (search-tree-file-absolute-path st file-index)
  (build-path
   (search-tree-root-path st)
   (hash-ref (search-tree-files st) file-index)))

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

;; from https://www.textfixer.com/tutorials/common-english-words.txt
(define stop-words (set "a" "able" "about" "across" "after" "all" "almost" "also" "am" "among" "an" "and" "any" "are""as" "at" "be" "because" "been" "but" "by" "can" "cannot" "could" "dear" "did" "do" "does" "either" "else" "ever" "every" "for" "from" "get" "got" "had" "has" "have" "he" "her" "hers" "him" "his" "how" "however" "i" "if" "in" "into" "is" "it" "its" "just" "least" "let" "like" "likely" "may" "me" "might" "most" "must" "my" "neither" "no" "nor" "not" "of" "off" "often" "on" "only" "or" "other" "our" "own" "rather" "said" "say" "says" "she" "should" "since" "so" "some" "than" "that" "the" "their" "them" "then" "there" "these" "they" "this" "tis" "to" "too" "twas" "us" "wants" "was" "we" "were" "what" "when" "where" "which" "while" "who" "whom" "why" "will" "with" "would" "yet" "you" "your"))

(define (stop-word? word)
  (define lc-word (string-downcase word))
  (if (set-member? stop-words lc-word)
      #t
      #f))

;; file-index is an index to the search tree's file list
(define (add-file-to-search-tree path file-index st)
  (define (insert-words words file-index trie)
    (cond
      [(empty? words) trie]
      ;; remove stop word checks for now
      #;[(stop-word? (car words))
       (insert-words (cdr words)
                     file-index
                     trie)]
      [else (insert-words (cdr words)
                          file-index
                          (insert-word (car words) file-index trie))]))

  (insert-words (file->word-list path) file-index (search-tree-trie st)))

(define (add-directory-to-search-tree path st)
  (define files (search-tree-files st))
  
  (for ([p (in-directory path)]
        [i 100000])
    (unless (directory-exists? p)
      ; add each file's relative path to the search tree's list of files, using 'i' as the key
      (define relpath (find-relative-path path p))
      (hash-set! files i relpath)
      (add-file-to-search-tree p i st))))

(define (read-hash-from-disk path offset)
  ; with-input-from-file won't let me set the file position for some reason
  (define in (open-input-file path))
  (file-position in offset)
  (let ([data (read in)])
    (close-input-port in)
    (deserialize data)))

;; retrieve a hash table of corpus matches for 'word' from the search tree
;; use force-copy to ensure that the hash table returned is a copy
(define (lookup-word-raw st word #:force-copy [copy-flag #f])
  (define cache-path (search-tree-node-datastore-path st))
  (define node (trie-lookup (string->list (string-downcase word))
                            (search-tree-trie st)))

  ;; the trie will either return a hash table or a file offset to a hash table
  ;; read the hash table from the cache file if necessary
  (cond
    [(not node) #f]
    [cache-path (read-hash-from-disk cache-path node)]
    [else
     (if copy-flag
         (hash-copy node)
         node)]))

;; retrieve matches from trie node's subtree as well
(define (lookup-word-plus-suffixes-raw st word)
  ;; need a copy of the hash table so we can do a mutable update to merge the results from suffixes
  (define ht (lookup-word-raw st word #:force-copy #t))

  ;; only match suffixes if the original word is in the trie.
  ;; may want to change this behavior but it doesn't seem right to treat
  ;; a search for "foob" as a search for "foob*" even if "foob" isn't in the corpus.
  (when ht
    (define key (string->list (string-downcase word)))
    (define cache-path (search-tree-node-datastore-path st))
    
    (for ([suffix (trie-get-sub-keys (search-tree-trie st) key #t)])
      (let* ([nd (trie-lookup (append key (string->list suffix))
                              (search-tree-trie st))]
             [node-hash (if (hash? nd) 
                            nd
                            (read-hash-from-disk cache-path nd))])
        (when node-hash 
          (hash-union! ht node-hash #:combine/key (lambda (k v1 v2) (+ v1 v2)))))))

  ht)

;; matches the quoted phrase exactly
;; returns a hash table
(define (lookup-phrase-raw st quoted-phrase)
  (define phrase (string-trim quoted-phrase "\""))
  (define query-list (string-split phrase))
  ;; run an AND query on all the words in the phrase to limit number of
  ;; files to search
  (define and-results
     (and-query
      st 
      (cdr query-list) 
      (lookup-word-raw st (car query-list))))

  (cond [(hash-empty? and-results) and-results]
        [(= (length query-list) 1) and-results]
        [else
         (define phrase-bytes (string->bytes/utf-8 phrase))
         (for ([(key value) (in-hash and-results)])
           (define num-matches-in-file (search-file-faster (search-tree-file-absolute-path st key) phrase-bytes))
           (if (= num-matches-in-file 0)
               (hash-remove! and-results key)
               (hash-set! and-results key num-matches-in-file)))
         and-results]))

(define (results-hash-to-list st ht)
  (if ht
      (hash-map ht
                (lambda (key value)
                  (cons (search-tree-file-absolute-path st key) value)))
      empty))
  
;; lookup the word in the trie
;; lookup-word-raw returns a hash table of results, using integer file indexes as keys
;; convert the hash table into a list of results, with the file index replaced with the file's path 
;; returns a list of (path . num-hits) or '() if no matches
(define (lookup-word st word)
  (define ht (lookup-word-raw st word))
  (results-hash-to-list st ht))

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
  (define ht (lookup-word-plus-suffixes-raw st word))
  (results-hash-to-list st ht))

;; matches the quoted phrase exactly
;; returns a list of (path . num-hits) or '() if no matches
(define (lookup-phrase st quoted-phrase)
  (define phrase (string-trim quoted-phrase "\""))
  (define query-list (string-split phrase))
  ;; run an AND query on all the words in the phrase to limit number of
  ;; files to search
  (define and-results
    (results-hash-to-list
     st
     (and-query
      st 
      (cdr query-list) 
      (lookup-word-raw st (car query-list)))))

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

;; calculates the intersection of h0 and h1, returning a new hash table
;; expects integer values and takes min value as new hash's value for each key
(define (hash-intersect-matches h0 h1)
  (define new-hash (make-hash))

  (for ([(key value) (in-hash h0)])
    (when (hash-has-key? h1 key)
      (hash-set! new-hash key (min value (hash-ref h1 key)))))

  new-hash)

(define (get-query-func query)
  (if (char=? (string-ref query 0) #\")
      lookup-phrase-raw
      lookup-word-plus-suffixes-raw))

(define (and-query st query-list result-ht)
  (cond 
    [(empty? query-list) result-ht]
    [else 
     (let* ([query-func (get-query-func (car query-list))]
            [new-matches (query-func st (car query-list))])
       (if new-matches
           (and-query st 
                      (cdr query-list)
                      (hash-intersect-matches result-ht new-matches))
           (make-hash)))]))

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
  (define query-func (get-query-func (car query-list)))
  (define results
    (results-hash-to-list
     st
     (and-query st 
                (cdr query-list) 
                (query-func st (car query-list)))))

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

;; dir is the path to the root directory containing the corpus of text files
(define (build-corpus dir)
  (define st (create-search-tree))
  (set-search-tree-root-path! st dir)
  (add-directory-to-search-tree dir st)
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

(define (load-corpus corpus-file-path corpus-root-path)
  (define st
    (call-with-input-file corpus-file-path
      (lambda (in) (deserialize (read in)))))

  (set-search-tree-root-path! st corpus-root-path)
  
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
      (define-values (base name dir?) (split-path corpus-file-path))
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
               path
               (search-tree-root-path st)))

(define (update-datastore-path! st path)
  (if (file-exists? path)
      (set-search-tree-node-datastore-path! st path)
      (error "File path does not exist!")))
