(asdf:defsystem #:also-alsa
  :description "Basic ALSA bindings for Common Lisp"
  :author "Eugene Zaikonnikov <eugene@funcall.org>"
  :license "LGPL"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "package")
               (:file "also-alsa")
	       (:file "also-mixer"))
  :in-order-to ((asdf:test-op (asdf:test-op "also-alsa/tests"))))

(asdf:defsystem #:also-alsa/tests
  :description "Hardware-free ALSA regression tests"
  :depends-on (#:also-alsa)
  :components ((:file "tests/read"))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:also-alsa-tests '#:run-tests)))
