#lang info

(define version "0.1")

(define collection 'games)

(define deps
  '("base"
    "struct-update-lib"
    "lens"
    "point-free"))

(define build-deps
  '("rackunit-spec"))
