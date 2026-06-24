;;; A very basic Advanced Linux Sound Architecture interface

(in-package #:also-alsa)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +epipe+ 32))

(declaim (inline alsa-element-type to-alsa-format ensure-success))

(defvar *alsa-warn* nil)

(define-foreign-library libasound
  (:unix "libasound.so.2")
  (t (:default "libasound.so")))

(defun load-alsa ()
  (use-foreign-library libasound))

;;; LW FLI can give us grief during cross builds
;;; We better invoke LOAD-ALSA as needed
#-lispworks(use-foreign-library libasound)

(defcenum snd-pcm-class
  (:snd-pcm-class-generic 0)
  :snd-pcm-class-multi
  :snd-pcm-class-modem
  :snd-pcm-class-digitizer)

(defcenum snd-pcm-stream
  (:snd-pcm-stream-playback 0)
  :snd-pcm-stream-capture)

(defcenum snd-pcm-access
  (:snd-pcm-access-mmap-interleaved 0)
  :snd-pcm-access-mmap-noninterleaved
  :snd-pcm-access-mmap-complex
  :snd-pcm-access-rw-interleaved
  :snd-pcm-access-rw-noninterleaved)

;; incomplete list of formats
(defcenum snd-pcm-format
  (:snd-pcm-format-unknown -1)
  (:snd-pcm-format-s8 0)
  :snd-pcm-format-u8
  :snd-pcm-format-s16-le
  :snd-pcm-format-s16-be
  :snd-pcm-format-u16-le
  :snd-pcm-format-u16-be
  :snd-pcm-format-s24-le
  :snd-pcm-format-s24-be
  :snd-pcm-format-u24-le
  :snd-pcm-format-u24-be
  :snd-pcm-format-s32-le
  :snd-pcm-format-s32-be
  :snd-pcm-format-u32-le
  :snd-pcm-format-u32-be
  :snd-pcm-format-float-le
  :snd-pcm-format-float-be
  :snd-pcm-format-float64-le
  :snd-pcm-format-float64-be
  :snd-pcm-format-iec958-subframe-le
  :snd-pcm-format-iec958-subframe-be
  :snd-pcm-format-mu-law
  :snd-pcm-format-a-law
  :snd-pcm-format-ima-adpcm
  :snd-pcm-format-mpeg
  :snd-pcm-format-gsm
  (:snd-pcm-format-special 31)
  (:snd-pcm-format-s24-3le 32)
  :snd-pcm-format-s24-3be
  :snd-pcm-format-u24-3le
  :snd-pcm-format-u24-3be)

(defcenum snd-pcm-state
  :snd-pcm-state-open
  :snd-pcm-state-setup
  :snd-pcm-state-prepared
  :snd-pcm-state-running
  :snd-pcm-state-xrun
  :snd-pcm-state-draining
  :snd-pcm-state-paused
  :snd-pcm-state-suspended
  :snd-pcm-state-disconnected)

(defctype snd-pcm-uframes :ulong)

(defctype snd-pcm-sframes :long)

(defcfun "snd_pcm_open" :int (pcm :pointer) (name :string) (stream snd-pcm-stream) (mode :int))

(defcfun "snd_pcm_state" snd-pcm-state (pcm :pointer))

(defcfun "snd_pcm_hw_params_malloc" :int (dptr :pointer))

(defcfun "snd_pcm_hw_params_any" :int (pcm :pointer) (params :pointer))

(defcfun "snd_pcm_hw_params_set_access" :int (pcm :pointer) (params :pointer) (access snd-pcm-access))

(defcfun "snd_pcm_hw_params_set_format" :int (pcm :pointer) (params :pointer) (format snd-pcm-format))

(defcfun "snd_pcm_hw_params_set_rate" :int (pcm :pointer) (params :pointer) (val :int) (dir :int))

(defcfun "snd_pcm_hw_params_set_channels" :int (pcm :pointer) (params :pointer) (val :int))

(defcfun "snd_pcm_hw_params_get_period_size" :int (params :pointer) (valp (:pointer snd-pcm-uframes)) (dir :pointer))

(defcfun "snd_pcm_hw_params" :int (pcm :pointer) (params :pointer))

(defcfun "snd_pcm_hw_params_free" :void (params :pointer))

(defcfun "snd_strerror" :string (val :int))

(defcfun "snd_pcm_prepare" :int (pcm :pointer))

(defcfun "snd_pcm_start" :int (pcm :pointer))

(defcfun "snd_pcm_drain" :int (pcm :pointer))

(defcfun "snd_pcm_drop" :int (pcm :pointer))

(defcfun "snd_pcm_close" :int (pcm :pointer))

(defcfun "snd_pcm_writei" snd-pcm-sframes (pcm :pointer) (buffer :pointer) (size snd-pcm-uframes))

(defcfun "snd_pcm_readi" snd-pcm-sframes (pcm :pointer) (buffer :pointer) (size snd-pcm-uframes))

(defcfun "snd_pcm_wait" :int (pcm :pointer) (timeout :int))

(defcfun "snd_pcm_delay" :int (pcm :pointer) (delayp (:pointer snd-pcm-sframes)))

(defcfun "snd_pcm_avail_delay" :int (pcm :pointer) (availp (:pointer snd-pcm-sframes)) (delayp (:pointer snd-pcm-sframes)))

(defcfun "snd_pcm_sw_params_malloc" :int (dptr :pointer))

(defcfun "snd_pcm_sw_params_free" :void (obj :pointer))

(defcfun "snd_pcm_sw_params_current" :int (pcm :pointer) (swparams :pointer))

(defcfun "snd_pcm_sw_params_get_start_threshold" :int (swparams :pointer) (pval (:pointer snd-pcm-uframes)))

(defcfun "snd_pcm_sw_params_set_start_threshold" :int (pcm :pointer) (swparams :pointer) (val snd-pcm-uframes))

(defcfun "snd_pcm_sw_params_set_avail_min" :int (pcm :pointer) (swparams :pointer) (val snd-pcm-uframes))

(defcfun "snd_pcm_sw_params" :int (pcm :pointer) (swparams :pointer))

(declaim (inline deref snd-pcm-writei snd-pcm-readi snd-pcm-avail-delay
		 snd-pcm-delay snd-pcm-drain snd-pcm-start snd-pcm-wait snd-pcm-prepare))

(defun deref (var)
  (mem-ref var :pointer))

(define-condition alsa-condition ()
  ())

(define-condition alsa-error (error alsa-condition)
  ((error-value :reader error-value
		:initarg :error-value)))

(define-condition alsa-call-error (alsa-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "ALSA error: ~A" (snd-strerror (error-value condition))))))

(define-condition alsa-general-error (alsa-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "ALSA error: ~A" (error-value condition)))))

(define-condition alsa-mixer-element-not-found (alsa-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "ALSA mixer element ~S not found"
		     (error-value condition)))))

(define-condition alsa-type-error (alsa-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "Invalid base type ~A" (error-value condition)))))

(defun ensure-success (value)
  (unless (zerop value)
    (error 'alsa-call-error :error-value value)))

(defun alsa-warn (string)
  (when *alsa-warn*
    (warn string)))

(defun make-pointer-cell ()
  (foreign-alloc :pointer :initial-contents (list (null-pointer))))

(defun ensure-pointer-cell (object slot-name)
  (unless (and (slot-boundp object slot-name)
	       (slot-value object slot-name))
    (setf (slot-value object slot-name) (make-pointer-cell))))

(defun ensure-pointer-cells (pcm)
  (ensure-pointer-cell pcm 'handle)
  (ensure-pointer-cell pcm 'params)
  (ensure-pointer-cell pcm 'swparams))

(defun free-pointer-target (cell free-function)
  (when cell
    (let ((target (deref cell)))
      (unless (null-pointer-p target)
	(funcall free-function target)
	(setf (mem-ref cell :pointer) (null-pointer))))))

(defun free-pointer-cell (object slot-name)
  (when (and (slot-boundp object slot-name)
	     (slot-value object slot-name))
    (foreign-free (slot-value object slot-name))
    (setf (slot-value object slot-name) nil)))

(defun free-pcm-params (pcm)
  (free-pointer-target (and (slot-boundp pcm 'params) (slot-value pcm 'params))
		       #'snd-pcm-hw-params-free)
  (free-pointer-target (and (slot-boundp pcm 'swparams) (slot-value pcm 'swparams))
		       #'snd-pcm-sw-params-free))

(defun free-pcm-pointer-cells (pcm)
  (free-pointer-cell pcm 'handle)
  (free-pointer-cell pcm 'params)
  (free-pointer-cell pcm 'swparams))

(defun close-pcm-handle (pcm)
  (let ((cell (and (slot-boundp pcm 'handle) (slot-value pcm 'handle))))
    (when cell
      (let ((handle (deref cell)))
	(unless (null-pointer-p handle)
	  (snd-pcm-close handle)
	  (setf (mem-ref cell :pointer) (null-pointer)))))))

(defun cleanup-pcm-open (pcm)
  (ignore-errors (free-pcm-params pcm))
  (ignore-errors (close-pcm-handle pcm))
  (setf (slot-value pcm 'status) :closed)
  (free-pcm-pointer-cells pcm))

(defclass pcm-stream ()
  ((handle :reader handle :initform (make-pointer-cell))
   (device :reader device :initarg :device)
   (params :reader params :initform (make-pointer-cell))
   (swparams :reader swparams :initform (make-pointer-cell))
   (pcm-format :reader pcm-format :initarg :pcm-format :initform :snd-pcm-format-s16-le)
   (buffer :reader buffer :initarg :buffer)
   (buffer-size :reader buffer-size :initarg :buffer-size)
   (element-type :reader element-type :initarg :element-type)
   (sample-rate :reader sample-rate :initarg :sample-rate :initform 44100)
   (direction :reader direction :initarg :direction)
   (channels-count :reader channels-count :initarg :channels-count)
   (status :accessor status :initform :initial :type (or :initial :open :closed))))

(defun alsa-element-type (type)
  (cond ((equalp type '(signed-byte 16)) :int16)
	((eql type 'single-float) :float)
        ((eql type 'double-float) :double)
        ((equalp type '(unsigned-byte 8)) :uint8)
        ((equalp type '(signed-byte 8)) :int8)
        ((equalp type '(unsigned-byte 16)) :uint16)
        ((equalp type '(unsigned-byte 32)) :uint32)
        ((equalp type '(signed-byte 32)) :int32)
        (t (error 'alsa-type-error :error-value type))))

(defun to-alsa-format (element-type)
  (cond ((eql element-type 'single-float) :snd-pcm-format-float-le)
        ((eql element-type 'double-float) :snd-pcm-format-float64-le)
        ((equalp element-type '(unsigned-byte 8)) :snd-pcm-format-u8)
        ((equalp element-type '(signed-byte 8)) :snd-pcm-format-s8)
        ((equalp element-type '(unsigned-byte 16)) :snd-pcm-format-u16-le)
        ((equalp element-type '(signed-byte 16)) :snd-pcm-format-s16-le)
        ((equalp element-type '(unsigned-byte 32)) :snd-pcm-format-u32-le)
        ((equalp element-type '(signed-byte 32)) :snd-pcm-format-s32-le)
        (t (error 'alsa-type-error :error-value element-type))))

(defun alsa-open-2 (pcs)
  (ensure-pointer-cells pcs)
  (handler-case
      (progn
	(ensure-success (snd-pcm-open (handle pcs) (device pcs) (direction pcs) 0))
	(setf (status pcs) :open)
	(ensure-success (snd-pcm-hw-params-malloc (params pcs)))
	(ensure-success (snd-pcm-hw-params-any (deref (handle pcs)) (deref (params pcs))))
	(ensure-success (snd-pcm-hw-params-set-access (deref (handle pcs)) (deref (params pcs)) :snd-pcm-access-rw-interleaved))
	(ensure-success (snd-pcm-hw-params-set-format (deref (handle pcs)) (deref (params pcs)) (pcm-format pcs)))
	(ensure-success (snd-pcm-hw-params-set-rate (deref (handle pcs)) (deref (params pcs)) (sample-rate pcs) 0))
	(ensure-success (snd-pcm-hw-params-set-channels (deref (handle pcs)) (deref (params pcs)) (channels-count pcs)))
	(ensure-success (snd-pcm-hw-params (deref (handle pcs)) (deref (params pcs))))

	(cffi:with-foreign-object (period 'snd-pcm-uframes)
	  (ensure-success (snd-pcm-hw-params-get-period-size (deref (params pcs)) period (cffi:null-pointer))))

	(free-pointer-target (params pcs) #'snd-pcm-hw-params-free)
	(ensure-success (snd-pcm-prepare (deref (handle pcs))))
    
	(ensure-success (snd-pcm-sw-params-malloc (swparams pcs)))
	(ensure-success (snd-pcm-sw-params-current (deref (handle pcs)) (deref (swparams pcs))))
	(ensure-success (snd-pcm-sw-params (deref (handle pcs)) (deref (swparams pcs))))
	pcs)
    (error (condition)
      (cleanup-pcm-open pcs)
      (error condition))))

(defun make-alsa-buffer (&key element-type size channels)
  (cffi:make-shareable-byte-vector (* (cffi:foreign-type-size (alsa-element-type element-type)) size channels)))

(defun alsa-open (device buffer-size element-type &key direction (sample-rate 44100) (channels-count 2) buffer)
  (let ((buffer-byte-size (* (cffi:foreign-type-size (alsa-element-type element-type))
			     buffer-size
			     channels-count)))
    (when buffer
      (assert (and (typep buffer '(simple-array (unsigned-byte 8) (*)))
		   (= (length buffer) buffer-byte-size)))))
  (let ((pcs (make-instance 'pcm-stream
			    :direction (case direction
					 (:input :snd-pcm-stream-capture)
					 (:output :snd-pcm-stream-playback))
			    :device device
			    :element-type element-type
			    :buffer (or buffer
					(make-alsa-buffer :element-type element-type :size buffer-size :channels channels-count))
			    :buffer-size (* buffer-size channels-count) ;number of samples really
			    :channels-count channels-count
			    :sample-rate sample-rate
			    :pcm-format (to-alsa-format element-type))))
    (alsa-open-2 pcs)))

(defgeneric alsa-reopen (pcs device buffer-size element-type &key direction sample-rate channels-count)
  (:documentation "Reopens the stream. If all parameters are the same, just keeps the exiting one."))

(defmethod alsa-reopen ((pcs pcm-stream) device buffer-size element-type &key direction (sample-rate 44100) (channels-count 2))
   (if (or (not (eql (status pcs) :open))
	   (not (and (equal device (device pcs))
		     (= (* buffer-size channels-count) (buffer-size pcs))
		     (equal element-type (element-type pcs))
		     (eql direction (case (direction pcs)
				      (:snd-pcm-stream-capture :input)
				      (:snd-pcm-stream-playback :output)))
		     (= sample-rate (sample-rate pcs))
		     (= channels-count (channels-count pcs)))))
       (progn
	 (when (eql (status pcs) :open)
	   (alsa-close pcs))
	 (alsa-open-2 (reinitialize-instance pcs
					     :direction (case direction
							  (:input :snd-pcm-stream-capture)
							  (:output :snd-pcm-stream-playback))
					     :device device
					     :element-type element-type
					     :buffer (cffi:make-shareable-byte-vector
						      (* (cffi:foreign-type-size (alsa-element-type element-type)) buffer-size channels-count))
					     #+nil(foreign-alloc (alsa-element-type element-type)
								    :count (* (cffi:foreign-type-size (alsa-element-type element-type)) buffer-size channels-count))
					     :buffer-size (* buffer-size channels-count) ;number of samples really
					     :channels-count channels-count
					     :sample-rate sample-rate
					     :pcm-format (to-alsa-format element-type))))
       (ensure-success (snd-pcm-drop (deref (handle pcs)))))
   pcs)

(defmethod ref ((pcm pcm-stream) position)
  (error "Deprecated method, use AREF on the (buffer pcm) instead"))

(defmethod (setf ref) (value (pcm pcm-stream) position)
  (error "Deprecated, use aref on the (buffer pcm) instead"))

(defmethod drain ((pcm pcm-stream))
  (ensure-success (snd-pcm-drain (deref (handle pcm))))
  pcm)

(defmethod drop ((pcm pcm-stream))
  (ensure-success (snd-pcm-drop (deref (handle pcm))))
  pcm)

(defmethod alsa-start ((pcm pcm-stream))
  (ensure-success (snd-pcm-start (deref (handle pcm))))
  pcm)

(defmethod alsa-resume ((pcm pcm-stream))
  (ensure-success (snd-pcm-prepare (deref (handle pcm))))
  pcm)

(defmethod get-delay ((pcm pcm-stream))
  (cffi:with-foreign-object (result 'snd-pcm-sframes)
    (let ((error-code (snd-pcm-delay (deref (handle pcm)) result)))
      (cond ((eql error-code (- +epipe+))
	     (alsa-warn "Pipe busted!") 0)
	    ((minusp error-code) (ensure-success error-code))
	    (t (mem-ref result 'snd-pcm-sframes))))))

(defmethod get-avail-delay ((pcm pcm-stream))
  (cffi:with-foreign-objects ((avail 'snd-pcm-sframes) (delay 'snd-pcm-sframes))
    (let ((error-code (snd-pcm-avail-delay (deref (handle pcm)) avail delay)))
      (cond ((eql error-code (- +epipe+)) (alsa-warn "Pipe busted!") (values 0 0))
	    ((minusp error-code) (ensure-success error-code))
	    (t (values (mem-ref avail 'snd-pcm-sframes)
		       (mem-ref delay 'snd-pcm-sframes)))))))

(defmethod alsa-close ((pcm pcm-stream))
  (when (eq (status pcm) :open)
    (let ((drain-result (snd-pcm-drain (deref (handle pcm)))))
      (ensure-success (snd-pcm-close (deref (handle pcm))))
      (setf (status pcm) :closed)
      (free-pcm-params pcm)
      (ensure-success drain-result)))
  (setf (status pcm) :closed)
  (free-pcm-pointer-cells pcm)
  pcm)

(defmethod alsa-wait ((pcm pcm-stream) &optional (timeout -1))
  (snd-pcm-wait (deref (handle pcm)) timeout))

(defun frame-byte-size (pcm)
  (* (channels-count pcm)
     (cffi:foreign-type-size (alsa-element-type (element-type pcm)))))

(defmethod alsa-write ((pcm pcm-stream))
  (assert (eql (direction pcm) :snd-pcm-stream-playback))
  (let ((expected (/ (buffer-size pcm) (channels-count pcm)))
	(frame-byte-size (frame-byte-size pcm)))
    (with-pointer-to-vector-data (ptr (buffer pcm))
      (loop with written = 0
	    while (< written expected)
	    for result = (snd-pcm-writei (deref (handle pcm))
					 (inc-pointer ptr (* written frame-byte-size))
					 (- expected written))
	    do (cond ((= result (- +epipe+))
		      (alsa-warn "Underrun!")
		      (ensure-success (snd-pcm-prepare (deref (handle pcm)))))
		     ((minusp result)
		      (ensure-success result))
		     ((zerop result)
		      (error 'alsa-general-error :error-value "snd_pcm_writei made no progress"))
		     (t
		      (incf written result))))))
  pcm)

(defmethod alsa-read ((pcm pcm-stream))
  (assert (eql (direction pcm) :snd-pcm-stream-capture))
  (let ((expected (/ (buffer-size pcm) (channels-count pcm)))
	(frame-byte-size (frame-byte-size pcm)))
    (with-pointer-to-vector-data (ptr (buffer pcm))
      (loop with frames-read = 0
	    while (< frames-read expected)
	    for result = (snd-pcm-readi (deref (handle pcm))
					(inc-pointer ptr (* frames-read frame-byte-size))
					(- expected frames-read))
	    do (cond ((= result (- +epipe+))
		      (alsa-warn "Underrun!")
		      (ensure-success (snd-pcm-prepare (deref (handle pcm)))))
		     ((minusp result)
		      (ensure-success result))
		     ((zerop result)
		      (error 'alsa-general-error :error-value "snd_pcm_readi made no progress"))
		     (t
		      (incf frames-read result))))))
  pcm)

(defmethod contents-to-lisp ((pcm pcm-stream))
  (let ((result (make-array (buffer-size pcm) :element-type (element-type pcm)))
	(alsa-type (alsa-element-type (element-type pcm))))
    (with-pointer-to-vector-data (ptr (buffer pcm))
      (loop for i from 0 below (buffer-size pcm) do
	   (setf (aref result i) (mem-aref ptr alsa-type i))))
    result))

(defmacro with-alsa-device ((stream device buffer-size element-type &key direction (sample-rate 44100) (channels-count 2) buffer) &body body)
  (assert direction)
  `(let ((,stream (also-alsa:alsa-open ,device ,buffer-size ,element-type
				       :buffer ,buffer
				       :direction ,direction :sample-rate ,sample-rate :channels-count ,channels-count)))
     (unwind-protect
	  (progn ,@body)
       (also-alsa:alsa-close ,stream))))

(defmacro with-alsa-buffer ((buffer pcm) &body body)
  `(cffi:with-pointer-to-vector-data (,buffer (buffer ,pcm))
     ,@body))

(defmethod get-state ((pcm pcm-stream))
  (snd-pcm-state (deref (handle pcm))))

(defmethod alsa-get-start-threshold ((pcm pcm-stream))
   (cffi:with-foreign-objects ((pval 'snd-pcm-uframes))
     (ensure-success
      (snd-pcm-sw-params-get-start-threshold (deref (swparams pcm)) pval))
     (mem-ref pval 'snd-pcm-uframes)))

(defmethod alsa-set-start-threshold ((pcm pcm-stream) value)
  (ensure-success
   (snd-pcm-sw-params-set-start-threshold (deref (handle pcm)) (deref (swparams pcm))
					  value)))
