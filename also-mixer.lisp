(in-package :also-alsa)

(defcfun "snd_mixer_open" :int (handle :pointer) (mode :int))

(defcfun "snd_mixer_attach" :int (handle :pointer) (card :string))

(defcfun "snd_mixer_selem_register" :int (handle :pointer) (options :pointer) (classp :pointer))

(defcfun "snd_mixer_load" :int (handle :pointer))

(defcfun "snd_mixer_selem_id_set_index" :void (sid :pointer) (val :uint))

(defcfun "snd_mixer_selem_id_set_name" :void (sid :pointer) (val :string))

(defcfun "snd_mixer_find_selem" :pointer (handle :pointer) (sid :pointer))

(defcfun "snd_mixer_selem_get_playback_volume_range" :int (selem :pointer) (min :pointer) (max :pointer))

(defcfun "snd_mixer_selem_get_playback_dB_range" :int (selem :pointer) (min :pointer) (max :pointer))

(defcfun "snd_mixer_selem_get_playback_dB" :int (selem :pointer) (value :pointer))

(defcfun "snd_mixer_selem_get_capture_volume_range" :int (selem :pointer) (min :pointer) (max :pointer))

(defcfun "snd_mixer_selem_get_capture_dB_range" :int (selem :pointer) (min :pointer) (max :pointer))

(defcfun "snd_mixer_selem_get_capture_dB" :int (selem :pointer) (value :pointer))

(defcfun "snd_mixer_selem_set_playback_volume_all" :int (selem :pointer) (volume :long))

(defcfun "snd_mixer_selem_set_capture_volume_all" :int (selem :pointer) (volume :long))

(defcfun "snd_mixer_selem_set_playback_dB_all" :int (selem :pointer) (volume :long) (dir :int))

(defcfun "snd_mixer_selem_set_capture_dB_all" :int (selem :pointer) (volume :long) (dir :int))

(defcfun "snd_mixer_close" :int (handle :pointer))

(defcstruct snd-mixer-selem-id
  (name :char :count 60)
  (index :uint))

(defun open-mixer (&optional (card "default"))
  (let ((handle (foreign-alloc :pointer :initial-contents (list (null-pointer)))))
    (ensure-success (snd-mixer-open handle 0))
    (ensure-success (snd-mixer-attach (deref handle) card))
    (ensure-success (snd-mixer-selem-register (deref handle) (null-pointer) (null-pointer)))
    (ensure-success (snd-mixer-load (deref handle)))
    handle))

(defun close-mixer (handle)
  (ensure-success (snd-mixer-close (deref handle)))
  (foreign-free handle))

(defun access-mixer-element (handle selem-name)
  (with-foreign-objects ((sid '(:struct snd-mixer-selem-id)))
    (snd-mixer-selem-id-set-index sid 0)
    (snd-mixer-selem-id-set-name sid selem-name)
    (snd-mixer-find-selem (deref handle) sid)))

(defun set-mixer-element-playback-volume (selem volume)
  (with-foreign-objects ((min :long)
			 (max :long))
    (ensure-success (snd-mixer-selem-get-playback-volume-range selem min max))
    (ensure-success (snd-mixer-selem-set-playback-volume-all selem (round (* volume (/ (mem-ref max :long) 100)))))))

(defun set-mixer-element-playback-db (selem db)
  (with-foreign-objects ((min :long)
			 (max :long))
    (ensure-success (snd-mixer-selem-get-playback-db-range selem min max))
    (let ((new-db (round (* db 100))))
      (if (<= (mem-ref min :long) new-db (mem-ref max :long))
	  (ensure-success (snd-mixer-selem-set-playback-db-all selem new-db 0))
	  (when *alsa-warn*
	    (warn "Supplied dB value ~d is out of range [~d , ~d]"
		new-db (mem-ref min :long) (mem-ref max :long)))))))

(defun set-mixer-element-capture-volume (selem volume)
  (with-foreign-objects ((min :long)
			 (max :long))
    (ensure-success (snd-mixer-selem-get-capture-volume-range selem min max))
    (ensure-success (snd-mixer-selem-set-capture-volume-all selem (round (* volume (/ (mem-ref max :long) 100)))))))

(defun set-mixer-element-capture-db (selem db)
  (with-foreign-objects ((min :long)
			 (max :long))
    (ensure-success (snd-mixer-selem-get-capture-db-range selem min max))
    (let ((new-db (round (* db 100))))
      (if (<= (mem-ref min :long) new-db (mem-ref max :long))
	  (ensure-success (snd-mixer-selem-set-capture-db-all selem new-db 0))
	  (when *alsa-warn*
	    (warn "Supplied dB value ~d is out of range [~d , ~d]"
		  new-db (mem-ref min :long) (mem-ref max :long)))))))

(defun set-mixer-element-volume (volume &key (element "Master") (direction :playback))
  (let ((handle (open-mixer)))
    (unwind-protect
	 (let ((selem (access-mixer-element handle element)))
	   (if (eql direction :playback)
	       (set-mixer-element-playback-volume selem volume)
	       (set-mixer-element-capture-volume selem volume)))
      (close-mixer handle))))
