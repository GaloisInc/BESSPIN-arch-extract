#!/usr/bin/env racket
#lang racket

(define args (vector->list (current-command-line-arguments)))

(define links-files (map (lambda (x) (string-append x "/links.rktd")) args))
(define pkgs-dirs args)

(define (update-config h)
  (hash-update (hash-update h
      'links-search-files (lambda (x) (append x links-files)) '(#f))
    'pkgs-search-dirs (lambda (x) (append x pkgs-dirs)) '(#f))
  )

(pretty-write (update-config (read)))
