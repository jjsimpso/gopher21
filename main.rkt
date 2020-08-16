#lang racket/base

(require racket/list)
(require racket/string)
(require racket/cmdline)
(require "corpus.rkt")
(require "gopher.rkt")

(define gopher21-version "0.5.1")

(define server-root (make-parameter "./"))
(define server-port (make-parameter 70))
(define search-tree-path (make-parameter #f))
(define disk-cache-path (make-parameter #f))
(define print-version (make-parameter #f))

(define arg-list
  (command-line
   #:once-any
   [("-c") path 
           "Create a corpus in the file <path>, from files in <arg-list> directories"
           (search-tree-path path)]
   [("-s") root port
           "Launch the server on <port>, serving from <root>.\n     <arg-list> is a list of the form <corpus name>:<path to corpus file>:<corpus root dir path>"
           (server-root root) (server-port (string->number port))]
   [("-v")
    "Print version number"
    (print-version #t)]
   
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
    (let ([corpus-w-paths (string-split arg ":")])
      (if (= (length corpus-w-paths) 3)
          (begin
            (printf "Loading corpus ~a~n" arg)
            (gopher-add-corpus (first corpus-w-paths) (second corpus-w-paths) (third corpus-w-paths)))
          (printf "Invalid corpus arg ~a, format is: <corpus name>:<path to corpus file>:<corpus root dir path>~n" arg)))))

(cond
  [(print-version)
   (printf "Version ~a~n" gopher21-version)]
  [(search-tree-path)
   (cond
     [(empty? arg-list)
      (error "Missing root directory of corpus.")]
     [(> 1 (length arg-list))
      (error "Too many arguments.")]
     [else
      (define corpus-root (car arg-list))
      (printf "Creating corpus from contents of ~a~n" corpus-root)
      
      (define st (build-corpus corpus-root))
      (printf "Finished building corpus~n")
      
      (if (disk-cache-path)
          (begin
            (printf "Creating disk cache in ~a~n" (disk-cache-path))
            (let ([dst (create-disk-cached-tree st (disk-cache-path))])
              (printf "Saving corpus to disk as ~a~n" (search-tree-path))
              (save-corpus dst (search-tree-path))))
          (begin
            (printf "Saving corpus to disk as ~a~n" (search-tree-path))
            (save-corpus st (search-tree-path))))])]
  [else
   (unless (empty? arg-list)
     (parse-corpus-paths arg-list))
   
   (printf "Starting server from ~a on port ~a...~n" (server-root) (server-port))
   (define stop (gopher-serve-map (server-root) (server-port)))
   (do-not-return)])
