;;;; package.lisp

(defpackage #:also-alsa
  (:use #:cl #:cffi)
  (:export #:pcm-stream #:alsa-open #:alsa-close #:ref #:alsa-write #:alsa-read)

