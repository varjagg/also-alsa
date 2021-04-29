(in-package :also-alsa)

(defcfun "snd_mixer_open" :int (handle :pointer) (mode :int))

(defcfun "snd_mixer_attach" :int (handle :pointer) (card :string))

(defcfun "snd_mixer_selem_register" :int (handle :pointer) (options :pointer) (classp :pointer))

(defcfun "snd_mixer_load" :int (handle :pointer))

(defcfun "snd_mixer_selem_id_set_index" :void (sid :pointer) (val :uint))

(defcfun "snd_mixer_selem_id_set_name" :void (sid :pointer) (val :string))

(defcfun "snd_mixer_find_selem" :pointer (handle :pointer) (sid :pointer))

(defcfun "snd_mixer_selem_get_playback_volume_range" :int (selem :pointer) (min :pointer) (max :pointer))

(defcfun "snd_mixer_selem_set_playback_volume_all" :int (selem :pointer) (volume :long))

(defcfun "snd_mixer_close" :int (handle :pointer))

(defcstruct snd-mixer-selem-id
  (name :char :count 60)
  (index :uint))

(defun set-master-volume (volume)
  (let ((handle (foreign-alloc :pointer :initial-contents (list (null-pointer))))
	selem
	(card "default")
	(selem-name "Master"))
    (ensure-success (snd-mixer-open handle 0))
    (ensure-success (snd-mixer-attach (deref handle) card))
    (ensure-success (snd-mixer-selem-register (deref handle) (null-pointer) (null-pointer)))
    (ensure-success (snd-mixer-load (deref handle)))
    (with-foreign-objects ((sid '(:struct snd-mixer-selem-id))
			   (min :long)
			   (max :long))
      (snd-mixer-selem-id-set-index sid 0)
      (snd-mixer-selem-id-set-name sid selem-name)
      (setf selem (snd-mixer-find-selem (deref handle) sid))
      (ensure-success (snd-mixer-selem-get-playback-volume-range selem min max))
      (ensure-success (snd-mixer-selem-set-playback-volume-all selem (round (* volume (/ (mem-ref max :long) 100))))))
    (ensure-success (snd-mixer-close (deref handle)))))

