;;;; package.lisp

(defpackage #:also-alsa
  (:use #:cl #:cffi)
  (:export #:pcm-stream #:alsa-open #:alsa-close #:ref #:alsa-write #:alsa-read
	   #:buffer #:direction #:element-type #:pcm-format #:channel-count #:buffer-size
	   #:alsa-element-type #:contents-to-lisp #:with-alsa-device #:alsa-reopen
	   #:get-delay #:get-avail-delay #:drain #:alsa-wait #:alsa-start
	   #:make-alsa-buffer #:alsa-resume #:*alsa-warn* #:alsa-warn))

