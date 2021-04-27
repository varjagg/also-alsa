(in-package :also-alsa)

(defcfun "snd_mixer_open" :int (handle :pointer) (mode :int))
(defcfun "snd_mixer_attach" :int (handle :pointer) (card :string))
(defcfun "snd_mixer_selem_register" :int (handle :pointer) (options :pointer) (classp :pointer))
(defcfun "snd_mixer_load" :int (handle :pointer))
;; a macro… (defcfun "snd_mixer_selem_id_alloca" :int (handle :pointer))
(defcfun "snd_mixer_close" :int (handle :pointer))

(defun set-master-volume (volume)
  (let ((handle (foreign-alloc :pointer :initial-contents (list (null-pointer))))
	(sid (foreign-alloc :pointer :initial-contents (list (null-pointer))))
	(min (cffi:foreign-alloc :int))
	(max (cffi:foreign-alloc :int))
	selem
	(card "default")
	(selem-name "Master"))
    (snd-mixer-open handle 0)
    (snd-mixer-attach (deref handle) card)
    (snd-mixer-selem-register handle (null-pointer) (null-pointer))
    (snd-mixer-load (deref handle))
    (snd-mixer-selem-id-alloca sid)
    (snd-mixer-selem-id-set-index sid 0)
    (snd-mixer-selem-id-set-name sid selem-name)
    (setf selem (snd-mixer-find-selem handle sid))

    (snd-mixer-selem-get-playback-volume-range selem min max)
    (snd-mixer-selem-set-playback-volume-all selem (round (* volume (/ (deref max) 100))))

    (snd-mixer-close (deref handle)))))

(defconstant +buffer-size+ 256)
(defconstant +sample-rate+ 8000)
(defconstant +channels+ 2)
(also-alsa:with-alsa-device (pcm "sysdefault" +buffer-size+ '(signed-byte 16) :direction :output
				 :channels-count +channels+ :sample-rate +sample-rate+)
  (also-alsa:alsa-start pcm)
  (loop repeat 5 do
       (loop with buffer = (also-alsa:buffer pcm)
	  for pos from 0 below (* +channels+ (also-alsa:buffer-size pcm)) by (* 2 +channels+)
	  for sample = (coerce (- (random 65535) 32767) '(signed-byte 16)) do
	    (setf (aref buffer pos) (logand sample #xff))
	    (setf (aref buffer (1+ pos)) (logand (ash sample -8) #xff))
	    (multiple-value-bind (avail delay) (also-alsa:get-avail-delay pcm)
	      (also-alsa:alsa-write pcm)))))
