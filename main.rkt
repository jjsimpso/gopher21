#lang racket/base

(require racket/list)
(require racket/string)
(require racket/cmdline)
(require "corpus.rkt")
(require "gopher.rkt")

(define server-root (make-parameter "./"))
(define server-port (make-parameter 70))
(define search-tree-path (make-parameter #f))
(define disk-cache-path (make-parameter #f))

(define arg-list
  (command-line
   #:once-any
   [("-c") path 
           "Create a corpus in the file <path>, from files in <arg-list> directories"
           (search-tree-path path)]
   [("-s") root port
           "Launch the server on <port>, serving from <root>.\n     <arg-list> is a list of the form <corpus name>:<corpus file>"
           (server-root root) (server-port (string->number port))]
   
   #:once-each
   [("-d") cache-path 
           "Create a disk cache in the file <cache-path>"
           (disk-cache-path cache-path)]

   [("-n") hname
           "Set the server's hostname to <hname>"
           (set-gopher-hostname hname)]

   #:args arg-list
   arg-list))

(define (parse-corpus-paths args)
  (for ([arg args])
    (let ([corpus-w-path (string-split arg ":")])
      (if (= (length corpus-w-path) 2)
          (begin
            (printf "Loading corpus ~a~n" arg)
            (gopher-add-corpus (first corpus-w-path) (second corpus-w-path)))
          (printf "Invalid corpus arg ~a, format is: <corpus name>:<corpus path>~n" arg)))))

(cond
  [(search-tree-path)
   (printf "Creating corpus from contents of: ")
   (for ([dir arg-list])
     (printf "~a " dir))
   (printf "~n")
   
   (define st (build-corpus arg-list))
   
   (printf "Finished building corpus~n")
   
   (if (disk-cache-path)
       (begin
         (printf "Creating disk cache in ~a~n" (disk-cache-path))
         (let ([dst (create-disk-cached-tree st (disk-cache-path))])
           (printf "Saving corpus to disk as ~a~n" (search-tree-path))
           (save-corpus dst (search-tree-path))))
       (begin
         (printf "Saving corpus to disk as ~a~n" (search-tree-path))
         (save-corpus st (search-tree-path))))]
  [else
   (unless (empty? arg-list)
     (parse-corpus-paths arg-list))
   
   (printf "Starting server from ~a on port ~a...~n" (server-root) (server-port))
   (define stop (gopher-serve-map (server-root) (server-port)))
   (do-not-return)])
