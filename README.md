# Basic ALSA bindings for Common Lisp

A small subset of ALSA interface for Common Lisp.

PCM streams are encapsulated in ALSO-ALSA:PCM-STREAM instances.

To play random noise on one channel of default stereo device:

```
(defconstant +buffer-size+ 256)
(defconstant +sample-rate+ 8000)
(defconstant +channels+ 2)

(also-alsa:with-alsa-device (pcm "sysdefault" +buffer-size+ '(signed-byte 16) :direction :output
                                                                              :channels-count +channels+ :sample-rate +sample-rate+)
  (also-alsa:alsa-start pcm)
  (loop with buffer = (also-alsa:buffer pcm)
        for pos from 0 below (also-alsa:buffer-size pcm)
        for sample = (coerce (- (random 65535) 32767) '(signed-byte 16)) do
          (setf (aref buffer pos) sample)
          (multiple-value-bind (avail delay) (also-alsa:get-avail-delay pcm)
            (also-alsa:alsa-write pcm))))
```
