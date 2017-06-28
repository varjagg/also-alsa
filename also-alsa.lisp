;;; A very basic Advanced Linux Sound Architecture interface

(in-package #:also-alsa)

(define-foreign-library libasound
  (:unix (:or "libasound.so"))
  (t (:default "libasound.so")))

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
