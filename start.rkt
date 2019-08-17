#lang racket

(require "gopher.rkt")

(gopher-add-corpus "GFA" "/opt/corpus/gfa.dst")

;(set-gopher-hostname "deepsea.org")
(printf "Starting server...~n")
(define stop (gopher-serve-map "/opt/ess/" 7070))
(do-not-return)
