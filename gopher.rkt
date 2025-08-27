#lang racket/base

(require racket/list)
(require racket/string)
(require racket/path)
(require racket/port)
(require racket/tcp)
(require racket/os)

(require "corpus.rkt")

(require (only-in "magic/image.rkt" (magic-query image-query)))
(require (only-in "magic/gif.rkt" (magic-query gif-query)))
(require (only-in "magic/html.rkt" (magic-query html-query)))

(provide gopher-serve-map)
(provide gopher-serve)
(provide do-not-return)
(provide set-gopher-hostname)
(provide gopher-add-corpus)

(define (do-not-return)
  (semaphore-wait (make-semaphore 0)))

;; gopher-hostname can be set to force the hostname in menu selectors
;; otherwise the server uses the ip address of the tcp connection
(define gopher-hostname null)
(define (set-gopher-hostname hostname)  (set! gopher-hostname hostname))

(define (get-hostname-or-ip ip)
  (if (null? gopher-hostname)
      ip
      gopher-hostname))

(define root-dir-path (make-parameter "/var/gopher/"))

(define-logger gopher)

; an assoc list (key is the name used to reference the corpus in querys, value is corpus search-tree object)
(define corpus-list '())

(define (gopher-add-corpus name path root-path)
  (with-handlers ([exn:fail? (lambda (v) 
                               (log-gopher-warning "Error loading corpus ~a" path)
                               #f)])
    (set! corpus-list (cons (cons name (load-corpus path root-path))
                            corpus-list))))

(define (gopher-serve-map root-dir port-no)
  (parameterize ([root-dir-path (simple-form-path (path->directory-path (string->path root-dir)))])
    (when (not (directory-exists? (root-dir-path)))
      (error "Path to root directory is invalid!"))
    
    (define main-cust (make-custodian))
    (parameterize ([current-custodian main-cust])
      (define listener (tcp-listen port-no 5 #t))
      (define (loop)
        (accept-and-handle-map listener)
        (loop))
      (log-gopher-info "Starting gopher server...")
      (thread loop))
    (lambda ()
      (log-gopher-info "Shutting down gopher server.")
      (custodian-shutdown-all main-cust))))

(define (accept-and-handle-map listener)
  (define cust (make-custodian))
  (define received-request? (box #f))
  
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread (lambda ()
              (handle-map in out received-request?)
              (close-input-port in)
              (close-output-port out))))
  ;; watcher thread: kill handler thread if we don't receive a valid request in 10 seconds
  (thread (lambda ()
            (sleep 10)
            (unless (unbox received-request?)
              (log-gopher-info "Watcher thread shutting down tcp connection")
              (custodian-shutdown-all cust)))))

;; handle a gopher request
(define (handle-map in out rr-box)
  (define-values (hostip port-no dip dport) (tcp-addresses in #t))

  (log-gopher-info "handling connection from ~a" dip)
  ;(printf "handling connection from ~a~n" dip)
  
  ;; read-line reads up to the first CRLF but doesn't include it in the returned string
  (define request (read-line in 'return-linefeed))
  ;; set parameter to indicate we received a request. prevents watcher thread from killing us.
  (set-box! rr-box #t)
  
  (if (eof-object? request)
      (send-error "Resource not found" out)
      (let* ([selector (parse-request-selector request)]
             ;; except for searches, assume selectors are file or directory names
             ;; use simplify path to remove updirs
             ;; resource will be an absolute path or a pair for a search request
             [resource 
              (cond
                [(string-prefix? selector "SEARCH:") (parse-search-request request)]
                [(or (= (string-length selector) 0)
                     (string=? selector "/"))
                 (root-dir-path)]
                [(absolute-path? selector) (simplify-path (build-path (root-dir-path) (substring selector 1)))]
                [else (simplify-path (build-path (root-dir-path) selector))])])
        
        (log-gopher-info "selector=~a, resource=~a~n" selector resource)
        (printf "selector=~a, resource=~a~n" selector resource)
        
        (cond 
          [(pair? resource) (handle-search-request resource (get-hostname-or-ip hostip) port-no out)]
          [(not (path-is-subdir? resource (root-dir-path))) (send-error "Resource not found" out)]
          [(and (file-exists? resource) (not (gophermap? resource)))
           (send-file resource out)]
          [(directory-exists? resource) (send-map resource (get-hostname-or-ip hostip) port-no out)] 
          [else (send-error "Resource not found" out)]))))

(define (handle-search-request resource hostname port-no out)
  (cond [(not (pair? resource)) 
         (send-error "Search Error!" out)]
        [(string=? (car resource) "*")
         (let ([query-string (cdr resource)])
           (printf "multi search for ~a~n" query-string)
           (send-search-results  (query-corpus-multi (map cdr corpus-list) query-string)
                                 query-string
                                 hostname 
                                 port-no 
                                 out))]
        [(not (assoc (car resource) corpus-list))
         (send-error (string-append "Corpus " (car resource) " not found") out)]
        [else
         (let ([corpus (cdr (assoc (car resource) corpus-list))]
               [query-string (cdr resource)])
           (cond
             [(not corpus) 
              (send-error (string-append "Invalid corpus: " (car resource)) out)]
             [else 
              (send-search-results (query-corpus corpus query-string) 
                                   query-string
                                   hostname 
                                   port-no 
                                   out)]))]))

(define (string-contains-char? str char)
  (for/or ([c (in-string str)])
    (if (char=? c char) #t #f)))

(define (send-map path hostname port-no out)
  (define map-file (build-path path "gophermap"))
  (if (file-exists? map-file)
      ; parse and send the gophermap line by line
      (call-with-input-file map-file 
        (lambda (in)
          (for ([line (in-lines in)])
            (if (string-contains-char? line #\tab)
                (send-map-resource-line line path hostname port-no out)
                (send-map-desc-line line out)))))
      ; send the directory listing
      (send-dir path hostname port-no out)))

(define (send-map-desc-line desc out)
  (send-line "i" desc "fake" "fake.host" 0 out))

(define (send-map-resource-line line map-dir-path hostname port-no out)
  (with-handlers
    ([exn:fail?
      (lambda (exn)
        (send-error-line "Malformed directory entry" out))])
    
    (define fields (string-split line "\t" #:repeat? #f))
    (define num-fields (length fields))
    
    (define type-char (string-ref line 0))
    (define name (substring (first fields) 1))
    (define selector (if (>= num-fields 2)
                         (second fields)
                         name))
    (define host (if (>= num-fields 3)
                     (third fields)
                     hostname))
    (define port (if (>= num-fields 4)
                     (fourth fields)
                     port-no))
    
    ;(printf "gophermap selector=~a, path=~a~n" selector map-dir-path)
    
    (if (>= num-fields 1)
        (send-line type-char name (build-map-selector selector map-dir-path (root-dir-path)) host port out)
        (send-error-line "Malformed directory entry" out))))

;; takes a selector from a gophermap file and processes it
;; will turn relative paths in the gophermap to full paths from the server root
(define (build-map-selector selector base-dir server-root-dir)
  (cond [(= (string-length selector) 0) ""]
        [(absolute-path? selector) selector]
        [(or (string-prefix? selector "URL:")
             (string-prefix? selector "SEARCH:")
             (string-prefix? selector "GET ")) selector]
        [else (string-append "/" 
                             (path->string (find-relative-path server-root-dir 
                                                               (simplify-path (path->complete-path selector base-dir)

                                                                              #f))))]))

(define (send-search-results results query-string hostname port-no out)
  (define (send-search-line path num-hits out)
    (define filename (find-relative-path (root-dir-path) path))
    (define desc (format "[~a hits] ~a" num-hits filename))
    (send-line "0" desc filename hostname port-no out))
  
  (send-map-desc-line (string-append "Search results for query: " query-string) out)
  (send-map-desc-line "" out)
  
  (if (not (empty? results))
      (begin
        (send-map-desc-line (format "~a files match the query" (length results)) out)
        (for ([result results])
          (send-search-line (car result) (cdr result) out)))
      (send-map-desc-line "No files match the search query" out)))

;; work around issue with the optimizer assuming bytes-utf-8-length returns a fixnum
(define non-broken-bytes-utf-8-length bytes-utf-8-length) 
;; To confuse and work around the optimizer:
(set! non-broken-bytes-utf-8-length non-broken-bytes-utf-8-length)

(define (is-binary? path)
  (call-with-input-file path
    (lambda (in)
      (define sample-bytes (read-bytes 2048 in))
      (if (eof-object? sample-bytes) #f
          (not (non-broken-bytes-utf-8-length sample-bytes))))))

(define (is-utf8-text? path)
  (call-with-input-file path
    (lambda (in)
      (define sample-bytes (read-bytes 2048 in))
      (if (eof-object? sample-bytes) #f
          (non-broken-bytes-utf-8-length sample-bytes)))))

(define (filetype-ext path)
  (define extension (filename-extension path))

  (cond [(directory-exists? path) "1"]
        [extension
         (cond [(or (bytes=? extension #"txt")
                    (bytes=? extension #"conf")
                    (bytes=? extension #"cfg")
                    (bytes=? extension #"sh")
                    (bytes=? extension #"bat")
                    (bytes=? extension #"ini"))"0"]
               [(or (bytes=? extension #"htm")
                    (bytes=? extension #"html")) "h"]
               [(bytes=? extension #"gif") "g"]
               [(or (bytes=? extension #"jpg")
                    (bytes=? extension #"jpeg")
                    (bytes=? extension #"bmp")
                    (bytes=? extension #"xpm")
                    (bytes=? extension #"ppm")
                    (bytes=? extension #"tiff")
                    (bytes=? extension #"png")) "I"]
               [(or (bytes=? extension #"wav")
                    (bytes=? extension #"ogg")
                    (bytes=? extension #"mp3")) "s"]
               [(or (bytes=? extension #"iso")
                    (bytes=? extension #"img")
                    (bytes=? extension #"ipf")
                    (bytes=? extension #"pdf")
                    (bytes=? extension #"exe")
                    (bytes=? extension #"com")) "9"]
               [(is-utf8-text? path) "0"]
               [else "9"])]
        [(is-utf8-text? path) "0"]
        [else "9"]))

(define (filetype path)
  (define extension (filename-extension path))

  (cond [(directory-exists? path) "1"]
        ; check for text extensions first since this covers the majority of gopher
        ; files. prevents having to run magic for every single text file.
        [(and extension
              (or (bytes=? extension #"txt")
                  (bytes=? extension #"TXT")
                  (bytes=? extension #"conf")
                  (bytes=? extension #"CONF")
                  (bytes=? extension #"cfg")
                  (bytes=? extension #"CFG")
                  (bytes=? extension #"sh")
                  (bytes=? extension #"bat")
                  (bytes=? extension #"BAT")
                  (bytes=? extension #"ini")
                  (bytes=? extension #"INI"))) "0"]
        [(with-input-from-file path image-query) "I"]
        [(with-input-from-file path gif-query) "g"]
        [(with-input-from-file path html-query) "h"]
        [(is-utf8-text? path) "0"]
        [extension
         (cond
           [(or (bytes=? extension #"wav")
                (bytes=? extension #"ogg")
                (bytes=? extension #"mp3")) "s"]
           [else "9"])]
        [else "9"]))

(define (send-dir path hostname port-no out)
  (define data-sent? #f)
  (for ([f (directory-list path)])
    (define file-path (build-path path f))
    (set! data-sent? #t)
    (send-line (filetype file-path) 
               (path->string f) 
               ; build an absolute path from the server root to the file
               (string-append "/" (path->string (find-relative-path (root-dir-path) file-path)))
               hostname port-no out))
  (unless data-sent?
    (send-line "i" "Sorry! Nothing here!" "fake" "(NULL)" 0 out)))

(define (string-prefix? s prefix)
  (regexp-match? (string-append "^" prefix) s))

(define (path-string path)
  (cond [(string? path) path]
        [(path? path) (path->string path)]
        [#t null]))

(define (path-is-subdir? path root)  
  (string-prefix? (if (path? path)
                      (path->string path)
                      path)
                  (if (path? root) 
                      (path->string root)
                      root)))


;; start a gopher server with: (define stop (gopher-serve test-table 7070))
(define (gopher-serve dispatch-table port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle dispatch-table listener)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))

(define (accept-and-handle dispatch-table listener)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread (lambda ()
              (handle dispatch-table in out)
              (close-input-port in)
              (close-output-port out))))
  ;; watcher thread
  (thread (lambda ()
            (sleep 10)
            (custodian-shutdown-all cust))))

(define (handle dispatch-table in out)
  (define-values (hostname port-no dip dport) (tcp-addresses in #t))
  
  (define (send-resource-list res out)
    (define (send-resource-line res out)
      (send-line (resource-type res) (resource-name res) (resource-selector res) 
                 (or (resource-host res) hostname) (or (resource-port res) port-no) out))
    (cond [(or (null? res) (empty? res))
           (display ".\r\n" out)]
          [(list? res) 
           (send-resource-line (car res) out)
           (send-resource-list (cdr res) out)]))


  ;; read-line reads up to the first CRLF but doesn't include it in the returned string
  ;; TODO: read-line can return eof, so fix that when I revisit this version of handle
  (define selector (parse-request-selector (read-line in 'return-linefeed)))
  (define res (hash-ref dispatch-table selector #f))

  ;; if request is in the dispatch table, then it is a menu list
  ;; else see if the file exists
  (if res
      (send-resource-list res out)
      (cond 
        [(file-exists? selector) (send-file selector out)]
        [else (send-error "Resource not found" out)])))

(define (parse-request-selector req)
  (define selector (string-split req "\t" #:trim? #f #:repeat? #f))
  (cond [(empty? selector) ""]
        [else (car selector)]))

;; returns #f if invalid search selector 
;; returns a pair with corpus name and query string if valid
(define (parse-search-request request)
  ; look for two substrings: the corpus and the query string
  ; also match the special character '*' to search all corpuses
  (define match (regexp-match #px"^SEARCH:(\\w+|\\*)\t(.+)$" request))
  (if match
      (cons (second match) (third match))
      #f))

(define (send-intro out)
  (send-line "i" "Welcome to Racket Gopher 0.1!" "fake" "(NULL)" 0 out)
  (display ".\r\n" out))

(define (send-error msg out)
  (display "An error occurred: " out)
  (display msg out)
  (display ".\r\n" out))

(define (send-error-line msg out)
  (send-line #\3 msg "fake" "error.host" 0 out))

(define (send-line type name selector host port out)
  (display type out)
  (display name out)
  (display "\t" out)
  (display selector out)
  (display "\t" out)
  (display host out)
  (display "\t" out)
  (display port out)
  (display "\r\n" out))

(define (send-file path out)
  (call-with-input-file path
    (lambda (in)
      (if in
          (copy-port in out) ; should it send a last line of "." as well?
          (send-error "Error reading file" out)))))

(struct resource
  (type
   name
   selector
   host   ; set host and port to #f for localhost
   port))

(define test-table (make-hash))
(hash-set! test-table "FAQS" (list (resource #\i
                                             "A collection of game walkthroughs:"
                                             "fake"
                                             "fake"
                                             0)
                                   (resource #\0
                                             "Castlevania Symphony of the Night A"
                                             "/home/jonathan/textfiles/gameinfo/castlevania_sotn_a.txt"
                                             #f
                                             #f)
                                   (resource #\0
                                             "Castlevania Symphony of the Night AA"
                                             "/home/jonathan/textfiles/gameinfo/castlevania_sotn_aa.txt"
                                             #f
                                             #f)
                                   (resource #\0
                                             "Castlevania Symphony of the Night B"
                                             "/home/jonathan/textfiles/gameinfo/castlevania_sotn_b.txt"
                                             #f
                                             #f)))

(hash-set! test-table "" (list (resource #\i "Welcome to Racket Gopher 0.1!" "fake" "nohost" 0)
                               (resource #\i "Have fun!" "fake" "nohost" 0)
                               (resource #\1 "Game FAQs" "FAQS" #f #f)))
