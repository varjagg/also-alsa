;;;; package.lisp

(defpackage #:also-alsa
  (:use #:cl #:cffi)
  (:export #:pcm-stream #:alsa-open #:alsa-close #:ref #:alsa-write #:alsa-read
	   #:buffer #:direction #:element-type #:pcm-format #:channel-count #:buffer-size
	   #:alsa-element-type #:contents-to-lisp #:with-alsa-device))

