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

