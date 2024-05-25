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

   ;; Event struct accessors
   event-type
   ;; Touch event slots
   touch@-x
   touch@-y
   touch@-time
   touch@-slot
   touch@-seat-slot
   ;; TODO: This is a bit ridiculous, since we have to define a slot
   ;; for at least 3 different event types.
   touch-up@-seat-slot
   touch-motion@-p

   ;; Pointer event slots
   pointer-motion@-dx
   pointer-motion@-dy

   pointer-button@-button
   pointer-button@-state

   pointer-scroll-finger@-dx
   pointer-scroll-finger@-dy

   ;; Keyboard event slots
   keyboard@-key
   keyboard@-state
   ))
