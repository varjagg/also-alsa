# Basic ALSA bindings for Common Lisp

A small subset of ALSA interface for Common Lisp.

PCM streams are encapsulated in ALSO-ALSA:PCM-STREAM instances.

`ALSO-ALSA:ALSA-READ` fills the stream's buffer and returns the PCM stream.
Its second value is true when an overrun (EPIPE) was recovered during that read.
Such a buffer may contain a gap or be discontinuous with the preceding buffer.

Timestamp functions require ALSA's 64-bit-time ABI (little-endian ARM32 or LP64).

Run the hardware-free regression tests with `(asdf:test-system :also-alsa)`.
Test failures signal an error; the tests do not open a PCM or mixer device.

To play random noise on one channel of default stereo device:

```
(defconstant +buffer-size+ 256)
(defconstant +sample-rate+ 8000)
(defconstant +channels+ 2)

(also-alsa:with-alsa-device (pcm "sysdefault" +buffer-size+ '(signed-byte 16) :direction :output
				 :channels-count +channels+ :sample-rate +sample-rate+)
  (also-alsa:alsa-start pcm)
  (loop repeat 2 do
       (loop with buffer = (also-alsa:buffer pcm)
	  for pos from 0 below (* +channels+ (also-alsa:buffer-size pcm)) by (* 2 +channels+)
	  for sample = (coerce (- (random 65535) 32767) '(signed-byte 16)) do
	    (setf (aref buffer pos) (logand sample #xff))
	    (setf (aref buffer (1+ pos)) (logand (ash sample -8) #xff))
	    (multiple-value-bind (avail delay) (also-alsa:get-avail-delay pcm)
	      (also-alsa:alsa-write pcm)))))

```
