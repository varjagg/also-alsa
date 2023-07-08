(asdf:defsystem #:also-alsa
  :description "Basic ALSA bindings for Common Lisp"
  :author "Eugene Zaikonnikov <eugene@funcall.org>"
  :license "LGPL"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "package")
               (:file "also-alsa")
	       (:file "also-mixer")))

