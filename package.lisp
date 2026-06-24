;;;; package.lisp

(defpackage #:also-alsa
  (:use #:cl #:cffi)
  (:export #:pcm-stream #:alsa-open #:alsa-close #:ref #:alsa-write #:alsa-read
	   #:buffer #:direction #:element-type #:pcm-format #:channels-count #:buffer-size
	   #:alsa-element-type #:contents-to-lisp #:with-alsa-device #:with-alsa-buffer #:alsa-reopen
	   #:get-delay #:get-avail-delay #:drain #:alsa-wait #:alsa-start #:drop
	   #:make-alsa-buffer #:alsa-resume #:*alsa-warn* #:alsa-warn #:get-state
	   #:open-mixer #:close-mixer #:access-mixer-element
	   #:set-mixer-element-playback-volume #:set-mixer-element-playback-db
	   #:set-mixer-element-capture-volume #:set-mixer-element-capture-db
	   #:set-mixer-element-volume #:get-mixer-element-playback-db
	   #:get-mixer-element-capture-db #:load-alsa #:error-value
	   #:alsa-condition #:alsa-error #:alsa-call-error #:alsa-general-error
	   #:alsa-mixer-element-not-found #:alsa-type-error
	   #:alsa-set-start-threshold #:alsa-get-start-threshold))
