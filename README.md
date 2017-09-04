# Basic ALSA bindings for Common Lisp

A small subset of ALSA interface for Common Lisp.

PCM streams are encapsulated in ALSO-ALSA:PCM-STREAM instances.

To play random noise on default stereo device:

```
(defconstant +buffer-size+ 256)
(defconstant +sample-rate+ 8000)
(defconstant +channels+ 2)


(also-alsa:with-alsa-device (pcm "default" +buffer-size+ '(signed-byte 16) :direction :output
	                                          :channels-count +channels+ :sample-rate +sample-rate+)
	(loop repeat 10 do
		(loop for pos from 0 below (* +channels+ (also-alsa:buffer-size pcm)) do
		(setf (also-alsa:ref pcm pos) (coerce (- (random 65535) 32767) '(signed-byte 16)))
		(also-alsa:alsa-write pcm))))

```
