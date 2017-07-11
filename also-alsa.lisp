;;; A very basic Advanced Linux Sound Architecture interface

(in-package #:also-alsa)

(define-foreign-library libasound
  (:unix "libasound.so")
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

(defctype snd-pcm-uframes :ulong)

(defctype snd-pcm-sframes :long)

(defcfun "snd_pcm_open" :int (pcm :pointer) (name :string) (stream snd-pcm-stream) (mode :int))

(defcfun "snd_pcm_hw_params_malloc" :int (dptr :pointer))

(defcfun "snd_pcm_hw_params_any" :int (pcm :pointer) (params :pointer))

(defcfun "snd_pcm_hw_params_set_access" :int (pcm :pointer) (params :pointer) (access snd-pcm-access))

(defcfun "snd_pcm_hw_params_set_format" :int (pcm :pointer) (params :pointer) (format snd-pcm-format))

(defcfun "snd_pcm_hw_params_set_rate" :int (pcm :pointer) (params :pointer) (val :int) (dir :int))

(defcfun "snd_pcm_hw_params_set_channels" :int (pcm :pointer) (params :pointer) (val :int))

(defcfun "snd_pcm_hw_params" :int (pcm :pointer) (params :pointer))

(defcfun "snd_pcm_hw_params_free" :int (params :pointer))

(defcfun "snd_strerror" :string (val :int))

(defcfun "snd_pcm_prepare" :int (pcm :pointer))

(defcfun "snd_pcm_close" :int (pcm :pointer))

(defcfun "snd_pcm_writei" snd-pcm-sframes (pcm :pointer) (buffer :pointer) (size snd-pcm-uframes))

(defcfun "snd_pcm_readi" snd-pcm-sframes (pcm :pointer) (buffer :pointer) (size snd-pcm-uframes))

(defun deref (var)
  (mem-ref var :pointer))

(defclass pcm-stream ()
  ((handle :reader handle :initform (foreign-alloc :pointer :initial-contents (list (null-pointer))))
   (params :reader params :initform (foreign-alloc :pointer :initial-contents (list (null-pointer))))
   (pcm-format :reader pcm-format :initarg :pcm-format :initform :snd-pcm-format-s16-le)
   (buffer :reader buffer :initarg :buffer)
   (buffer-size :reader buffer-size :initarg :buffer-size)
   (element-type :reader element-type :initarg :element-type)
   (sample-rate :reader sample-rate :initarg :sample-rate :initform 44100)
   (direction :reader direction :initarg :direction)))

(defun alsa-element-type (type)
  (ecase type
    (single-float :float)
    (double-float :double)
    (((unsigned-byte 8)) :uint8)
    (((signed-byte 8)) :int8)
    (((unsigned-byte 16)) :uint16)
    (((signed-byte 16)) :int16)))

(defun alsa-open (device buffer-size element-type &key direction)
  (let ((pcs (make-instance 'pcm-stream
			    :direction (case direction
					 (:input :snd-pcm-stream-capture)
					 (:output :snd-pcm-stream-playback))
			    :element-type element-type
			    :buffer (foreign-alloc (alsa-element-type element-type) :count buffer-size) :buffer-size buffer-size
			    :pcm-format (ecase element-type
					  (single-float :snd-pcm-format-float-le)
					  (double-float :snd-pcm-format-float64-le)
					  (((unsigned-byte 8)) :snd-pcm-format-u8-le)
					  (((signed-byte 8)) :snd-pcm-format-s8-le)
					  (((unsigned-byte 16) :snd-pcm-format-u16-le))
					  (((signed-byte 16)) :snd-pcm-format-s16-le)))))
    
    (snd-pcm-open (handle pcs) device (direction pcs) 0)
    (snd-pcm-hw-params-malloc (params pcs))
    (snd-pcm-hw-params-any (deref (handle pcs)) (deref (params pcs)))
    (snd-pcm-hw-params-set-access (deref (handle pcs)) (deref (params pcs)) :snd-pcm-access-rw-interleaved)
    (snd-pcm-hw-params-set-format (deref (handle pcs)) (deref (params pcs)) (pcm-format pcs))
    (snd-pcm-hw-params-set-rate (deref (handle pcs)) (deref (params pcs)) (sample-rate pcs) 0)
    (snd-pcm-hw-params-set-channels (deref (handle pcs)) (deref (params pcs)) 2)
    (snd-pcm-hw-params (deref (handle pcs)) (deref (params pcs)))
    (snd-pcm-hw-params-free (deref (params pcs)))
    pcs))

(defmethod ref ((pcm pcm-stream) position)
  (mem-aref (buffer pcm) (alsa-element-type (element-type pcm)) position))

(defmethod alsa-close ((pcm pcm-stream))
  (snd-pcm-close (deref (handle pcm))))

(defmethod alsa-write ((pcm pcm-stream))
b  (assert (eql (direction pcm) :output))
  (with-slots (buffer element-type) pcm))

