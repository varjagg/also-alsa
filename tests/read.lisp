(defpackage #:also-alsa-tests
  (:use #:cl)
  (:export #:run-tests))

(in-package #:also-alsa-tests)

(defun make-mocked-reader ()
  ;; READI/PREPARE are declared inline in production. Compile the actual method
  ;; body with lexical foreign-call replacements, without replacing any global
  ;; functions or risking a real ALSA call with a fake handle.
  (let* ((*package* (find-package '#:also-alsa))
         (form (find-if
                (lambda (form)
                  (and (consp form) (eq (first form) 'defmethod)
                       (eq (second form) 'also-alsa:alsa-read)))
                (uiop:read-file-forms
                 (asdf:system-relative-pathname "also-alsa" "also-alsa.lisp")))))
    (assert form)
    (assert (equal (third form) '((also-alsa::pcm also-alsa:pcm-stream))))
    (compile nil
             `(lambda (also-alsa::pcm read-function prepare-function)
                (flet ((also-alsa::snd-pcm-readi (handle pointer frames)
                         (funcall read-function handle pointer frames))
                       (also-alsa::snd-pcm-prepare (handle)
                         (funcall prepare-function handle)))
                  ,@(cdddr form))))))

(defun run-read-case (reader script expected-calls expected-flags expected-prepares
                      &key (channels 1) (prepare-result 0) expected-error)
  (let* ((sample-count (* 4 channels))
         (pcm (make-instance 'also-alsa:pcm-stream
                             :direction :snd-pcm-stream-capture
                             :element-type '(signed-byte 16)
                             :channels-count channels
                             :buffer-size sample-count
                             :buffer (also-alsa:make-alsa-buffer
                                      :element-type '(signed-byte 16)
                                      :size 4 :channels channels)))
         (remaining (copy-list script))
         (calls nil) (prepares 0) (sample 0) base-address caught
         (also-alsa:*alsa-warn* nil))
    (unwind-protect
         (flet ((read-buffer (handle pointer frames)
                  (assert (cffi:null-pointer-p handle))
                  (assert remaining)
                  (unless base-address
                    (setf base-address (cffi:pointer-address pointer)))
                  (push (list (- (cffi:pointer-address pointer) base-address) frames) calls)
                  (let ((result (pop remaining)))
                    (when (plusp result)
                      (assert (<= result frames))
                      (dotimes (index (* result channels))
                        (setf (cffi:mem-aref pointer :int16 index) (incf sample))))
                    result))
                (prepare-buffer (handle)
                  (assert (cffi:null-pointer-p handle))
                  (incf prepares)
                  prepare-result))
           (handler-case
               (dolist (expected-flag expected-flags)
                 (let ((values (multiple-value-list
                                (funcall reader pcm #'read-buffer #'prepare-buffer))))
                   (assert (= 2 (length values)))
                   (assert (eq pcm (first values)))
                   (assert (eql expected-flag (second values)))
                   (assert (equal (coerce (also-alsa:contents-to-lisp pcm) 'list)
                                  (loop for value from (1+ (- sample sample-count))
                                        to sample collect value)))))
             (also-alsa:alsa-error (condition)
               (setf caught condition)))
           (if expected-error
               (assert (typep caught expected-error))
               (assert (null caught)))
           (assert (null remaining))
           (assert (equal expected-calls (nreverse calls)))
           (assert (= expected-prepares prepares)))
      (also-alsa::free-pcm-pointer-cells pcm))))

(defun run-timestamp-layout-tests ()
  (let ((type '(:struct also-alsa::snd-htimestamp)))
    (assert (= 16 (cffi:foreign-type-size type)))
    (assert (= 0 (cffi:foreign-slot-offset type 'also-alsa::seconds)))
    (assert (= 8 (cffi:foreign-slot-offset type 'also-alsa::nanoseconds)))
    (cffi:with-foreign-object (timestamp :uint64 2)
      (dolist (seconds '(0 3236 2147483648 4294967297))
        (dolist (nanoseconds '(0 1 656748502 999999999))
          ;; Leave ARM32's trailing padding nonzero: it is not part of tv_nsec.
          (dotimes (index 16)
            (setf (cffi:mem-aref timestamp :uint8 index) #xa5))
          (setf (cffi:mem-ref timestamp :int64) seconds
                (cffi:mem-ref timestamp :long 8) nanoseconds)
          (assert (= seconds
                     (cffi:foreign-slot-value timestamp type 'also-alsa::seconds)))
          (assert (= nanoseconds
                     (cffi:foreign-slot-value timestamp type 'also-alsa::nanoseconds)))))))
  (format t "~&Passed 16 time64 timestamp layout cases.~%"))

(defun run-tests ()
  (run-timestamp-layout-tests)
  (let ((reader (make-mocked-reader)))
    (run-read-case reader '(4) '((0 4)) '(nil) 0)
    (run-read-case reader '(1 2 1) '((0 4) (2 3) (6 1)) '(nil) 0)
    (run-read-case reader '(-32 4) '((0 4) (0 4)) '(t) 1)
    (run-read-case reader '(1 -32 3) '((0 4) (2 3) (2 3)) '(t) 1)
    (run-read-case reader '(-32 1 -32 3) '((0 4) (0 4) (2 3) (2 3)) '(t) 2)
    (run-read-case reader '(1 -32 3) '((0 4) (4 3) (4 3)) '(t) 1 :channels 2)
    (run-read-case reader '(-32 4 4) '((0 4) (0 4) (0 4)) '(t nil) 1)
    (run-read-case reader '(-5) '((0 4)) '(nil) 0 :expected-error 'also-alsa:alsa-call-error)
    (run-read-case reader '(0) '((0 4)) '(nil) 0 :expected-error 'also-alsa:alsa-general-error)
    (run-read-case reader '(-32) '((0 4)) '(nil) 1
                   :prepare-result -5 :expected-error 'also-alsa:alsa-call-error))
  (format t "~&Passed 10 hardware-free ALSA read regression cases.~%")
  t)
