;;;; package.lisp

(defpackage #:libinput
  (:use #:cl #:cffi)
  (:export
   unref
   create-context
   path-add-device
   path-remove-device
   get-fd
   device-ref
   device-unref
   device-cap-keyboard
   device-cap-pointer
   device-cap-touch
   device-cap-tablet-tool
   device-cap-tablet-pad
   device-cap-gesture
   device-has-capability
   device-get-name
   get-event

   touch@-x
   touch@-y
   touch@-time
   touch@-slot
   touch@-seat-slot
   touch-motion@-p))
