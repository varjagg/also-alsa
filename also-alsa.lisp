;;; A very basic Advanced Linux Sound Architecture interface

(in-package #:also-alsa)

(defcenum snd-pcm-class
  (:snd-pcm-class-generic 0)
  :snd-pcm-class-multi
  :snd-pcm-class-modem
  :snd-pcm-class-digitizer)

(defcenum snd-pcm-stream
  (:snd-pcm-stream-playback 0)
  :snd-pcm-stream-capture)
