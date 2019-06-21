;;; A very basic Advanced Linux Sound Architecture interface

(in-package #:also-alsa)

(eval-when (:compile-toplevel)
  (defconstant +epipe+ 32))

(define-foreign-library libasound
  (:unix "libasound.so.2")
  (t (:default "libasound.so")))

(use-foreign-library libasound)

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

(defcfun "snd_pcm_hw_params_get_period_size" :int (pcm :pointer) (valp (:pointer :ulong)) (dir :pointer))

(defcfun "snd_pcm_hw_params" :int (pcm :pointer) (params :pointer))

(defcfun "snd_pcm_hw_params_free" :int (params :pointer))

(defcfun "snd_strerror" :string (val :int))

(defcfun "snd_pcm_prepare" :int (pcm :pointer))

(defcfun "snd_pcm_start" :int (pcm :pointer))

(defcfun "snd_pcm_drain" :int (pcm :pointer))

(defcfun "snd_pcm_drop" :int (pcm :pointer))

(defcfun "snd_pcm_close" :int (pcm :pointer))

(defcfun "snd_pcm_writei" snd-pcm-sframes (pcm :pointer) (buffer :pointer) (size snd-pcm-uframes))

(defcfun "snd_pcm_readi" snd-pcm-sframes (pcm :pointer) (buffer :pointer) (size snd-pcm-uframes))

(defcfun "snd_pcm_delay" :int (pcm :pointer) (delayp (:pointer :long)))

(defcfun "snd_pcm_sw_params_malloc" :int (dptr :pointer))

(defcfun "snd_pcm_sw_params_current" :int (pcm :pointer) (swparams :pointer))

(defcfun "snd_pcm_sw_params_get_start_threshold" :int (pcm :pointer) (swparams :pointer) (pval (:pointer :ulong)))

(defcfun "snd_pcm_sw_params_set_start_threshold" :int (pcm :pointer) (swparams :pointer) (val :ulong))

(defcfun "snd_pcm_sw_params_set_avail_min" :int (pcm :pointer) (swparams :pointer) (val :ulong))

(defcfun "snd_pcm_sw_params" :int (pcm :pointer) (swparams :pointer))

(defun deref (var)
  (mem-ref var :pointer))

(defun ensure-success (value)
  (unless (zerop value)
    (error "ALSA error: ~A" (snd-strerror value))))

(defclass pcm-stream ()
  ((handle :reader handle :initform (foreign-alloc :pointer :initial-contents (list (null-pointer))))
   (device :reader device :initarg :device)
   (params :reader params :initform (foreign-alloc :pointer :initial-contents (list (null-pointer))))
   (swparams :reader swparams :initform (foreign-alloc :pointer :initial-contents (list (null-pointer))))
   (pcm-format :reader pcm-format :initarg :pcm-format :initform :snd-pcm-format-s16-le)
   (buffer :reader buffer :initarg :buffer)
   (buffer-size :reader buffer-size :initarg :buffer-size)
   (element-type :reader element-type :initarg :element-type)
   (sample-rate :reader sample-rate :initarg :sample-rate :initform 44100)
   (direction :reader direction :initarg :direction)
   (channels-count :reader channels-count :initarg :channels-count)
   (status :accessor status :initform :initial :type (or :initial :open :closed))))

(defun alsa-element-type (type)
  (cond ((eql type 'single-float) :float)
        ((eql type 'double-float) :double)
        ((equalp type '(unsigned-byte 8)) :uint8)
        ((equalp type '(signed-byte 8)) :int8)
        ((equalp type '(unsigned-byte 16)) :uint16)
        ((equalp type '(signed-byte 16)) :int16)
        ((equalp type '(unsigned-byte 32)) :uint32)
        ((equalp type '(signed-byte 32)) :int32)
        (t (error "Invalid base type ~A" type))))

(defun to-alsa-format (element-type)
  (cond ((eql element-type 'single-float) :snd-pcm-format-float-le)
        ((eql element-type 'double-float) :snd-pcm-format-float64-le)
        ((equalp element-type '(unsigned-byte 8)) :snd-pcm-format-u8)
        ((equalp element-type '(signed-byte 8)) :snd-pcm-format-s8)
        ((equalp element-type '(unsigned-byte 16)) :snd-pcm-format-u16-le)
        ((equalp element-type '(signed-byte 16)) :snd-pcm-format-s16-le)
        ((equalp element-type '(unsigned-byte 32)) :snd-pcm-format-u32-le)
        ((equalp element-type '(signed-byte 32)) :snd-pcm-format-s32-le)
        (t (error "Invalid base type ~A" element-type))))

(defun alsa-open-2 (pcs)
  (ensure-success (snd-pcm-open (handle pcs) (device pcs) (direction pcs) 0))
  (setf (status pcs) :open)
  (ensure-success (snd-pcm-hw-params-malloc (params pcs)))
  (ensure-success (snd-pcm-hw-params-any (deref (handle pcs)) (deref (params pcs))))
  (ensure-success (snd-pcm-hw-params-set-access (deref (handle pcs)) (deref (params pcs)) :snd-pcm-access-rw-interleaved))
  (ensure-success (snd-pcm-hw-params-set-format (deref (handle pcs)) (deref (params pcs)) (pcm-format pcs)))
  (ensure-success (snd-pcm-hw-params-set-rate (deref (handle pcs)) (deref (params pcs)) (sample-rate pcs) 0))
  (ensure-success (snd-pcm-hw-params-set-channels (deref (handle pcs)) (deref (params pcs)) (channels-count pcs)))
  (ensure-success (snd-pcm-hw-params (deref (handle pcs)) (deref (params pcs))))

  (cffi:with-foreign-object (period :uint)
    (ensure-success (snd-pcm-hw-params-get-period-size (deref (params pcs)) period (cffi:null-pointer)))
    #+nil(format t "Period size: ~D" (mem-ref period :uint)))

  (snd-pcm-hw-params-free (deref (params pcs)))
  (ensure-success (snd-pcm-prepare (deref (handle pcs))))
    
  (ensure-success (snd-pcm-sw-params-malloc (swparams pcs)))
  (ensure-success (snd-pcm-sw-params-current (deref (handle pcs)) (deref (swparams pcs))))
  (ensure-success (snd-pcm-sw-params (deref (handle pcs)) (deref (swparams pcs))))
  pcs)

(defun alsa-open (device buffer-size element-type &key direction (sample-rate 44100) (channels-count 2))
  (let ((pcs (make-instance 'pcm-stream
			    :direction (case direction
					 (:input :snd-pcm-stream-capture)
					 (:output :snd-pcm-stream-playback))
			    :device device
			    :element-type element-type
			    :buffer (foreign-alloc (alsa-element-type element-type)
						   :count (* (cffi:foreign-type-size (alsa-element-type element-type)) buffer-size channels-count))
			    :buffer-size (* buffer-size channels-count) ;number of samples really
			    :channels-count channels-count
			    :sample-rate sample-rate
			    :pcm-format (to-alsa-format element-type))))
    (alsa-open-2 pcs)))

(defgeneric alsa-reopen (pcs device buffer-size element-type &key direction sample-rate channels-count)
  (:documentation "Reopens the stream. If all parameters are the same, just keeps the exiting one."))

(defmethod alsa-reopen ((pcs pcm-stream) device buffer-size element-type &key direction (sample-rate 44100) (channels-count 2))
   (if (or (eql (status pcs) :initial)
	   (not (and (equal device (device pcs))
		     (= (* buffer-size channels-count) (buffer-size pcs))
		     (equal element-type (element-type pcs))
		     (eql direction (direction pcs))
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
					     :buffer (foreign-alloc (alsa-element-type element-type)
								    :count (* (cffi:foreign-type-size (alsa-element-type element-type)) buffer-size channels-count))
					     :buffer-size (* buffer-size channels-count) ;number of samples really
					     :channels-count channels-count
					     :sample-rate sample-rate
					     :pcm-format (to-alsa-format element-type))))
       (snd-pcm-drop (deref (handle pcs))))
   pcs)

(defmethod ref ((pcm pcm-stream) position)
  (mem-aref (buffer pcm) (alsa-element-type (element-type pcm)) position))

(defmethod (setf ref) (value (pcm pcm-stream) position)
  #+(or)(assert (eql (element-type pcm) (type-of value)))
  (setf (mem-aref (buffer pcm) (alsa-element-type (element-type pcm)) position) value))

(defmethod drain ((pcm pcm-stream))
  (snd-pcm-drain (deref (handle pcm))))

(defmethod get-delay ((pcm pcm-stream))
  (snd-pcm-prepare (deref (handle pcm)))
  (cffi:with-foreign-object (result :long)
    (let ((error-code (snd-pcm-delay (deref (handle pcm)) result)))
      (cond ((eql error-code (- +epipe+)) (format t "Pipe busted!") 0)
	    ((minusp error-code) (error "ALSA error: ~A" error-code))
	  (t (mem-ref result :uint))))))

(defmethod alsa-close ((pcm pcm-stream))
  (when (eq (status pcm) :open)
    (snd-pcm-drain (deref (handle pcm)))
    (snd-pcm-close (deref (handle pcm)))
    (cffi:foreign-free (buffer pcm)))
  (setf (status pcm) :closed)
  pcm)

(defmethod alsa-write ((pcm pcm-stream))
  (assert (eql (direction pcm) :snd-pcm-stream-playback))
  (let* ((expected (/ (buffer-size pcm) (channels-count pcm)))
         (result (snd-pcm-writei (deref (handle pcm)) (buffer pcm) expected)))
    (cond ((= result (- +epipe+))
           ;; Under run, so prepare and retry
	   (format t "Underrun!")
           (snd-pcm-prepare (deref (handle pcm)))
           (alsa-write pcm))
          ((/= result expected)
           (error "ALSA error: ~A" result)))))

(defmethod alsa-read ((pcm pcm-stream))
  (assert (eql (direction pcm) :snd-pcm-stream-capture))
  (let ((result (snd-pcm-readi (deref (handle pcm)) (buffer pcm) (/ (buffer-size pcm) (channels-count pcm)))))
    (unless (= result (/ (buffer-size pcm) (channels-count pcm)))
      (if (eql result (- +epipe+))
	  (progn (format t "Underrun!") (snd-pcm-prepare (deref (handle pcm))))
	  (error "ALSA error: ~A" result)))
    pcm))

(defmethod contents-to-lisp ((pcm pcm-stream))
  (let ((result (make-array (buffer-size pcm) :element-type (element-type pcm))))
    (loop for i from 0 below (buffer-size pcm) do
	 (setf (aref result i) (ref pcm i)))
    result))

(defmacro with-alsa-device ((stream device buffer-size element-type &key direction (sample-rate 44100) (channels-count 2)) &body body)
  (assert direction)
  `(let ((,stream (also-alsa:alsa-open ,device ,buffer-size ,element-type
				       :direction ,direction :sample-rate ,sample-rate :channels-count ,channels-count)))
     (unwind-protect
	  (progn ,@body)
       (also-alsa:drain ,stream)
       (also-alsa:alsa-close ,stream))))
