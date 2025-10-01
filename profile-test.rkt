#lang racket

(require profile)
(require "corpus.rkt")

(define ps2 (load-corpus "/home/jonathan/ps2.corpus" "/data/jonathan/ess/gamefaqs-archive/ps2"))

(profile-thunk (thunk (query-corpus ps2 "dragon wars")))
