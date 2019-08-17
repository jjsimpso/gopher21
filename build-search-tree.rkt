#lang racket

(require racket/cmdline)
(require "corpus.rkt")

(define search-tree-path (make-parameter #f))
(define disk-cache-path (make-parameter #f))

(define dir-list
  (command-line
   #:once-each
   [("-o") path 
           "Output file name"
           (search-tree-path path)]
   [("-d") cache-path 
           "Create a disk cache with file name"
           (disk-cache-path cache-path)]

   #:args dir-list
   dir-list))

(unless (search-tree-path) (error "Must specify output path with -o"))

(printf "Creating corpus from contents of: ")
(for ([dir dir-list])
  (printf "~a " dir))
(printf "~n")

(define st (build-corpus dir-list))

(printf "Finished building corpus~n")

(if (disk-cache-path)
    (begin
      (printf "Creating disk cache in ~a~n" (disk-cache-path))
      (let ([dst (create-disk-cached-tree st (disk-cache-path))])
        (printf "Saving corpus to disk as ~a~n" (search-tree-path))
        (save-corpus dst (search-tree-path))))
    (begin
      (printf "Saving corpus to disk as ~a~n" (search-tree-path))
      (save-corpus st (search-tree-path))))
