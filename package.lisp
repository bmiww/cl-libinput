;;;; package.lisp

(defpackage #:libinput
  (:use #:cl #:cffi)
  (:export
   create-context
   unref suspend

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
   device-get-name
   device-has-capability
   device-get-size
   device-get-output-name
   device-get-id-product
   device-get-id-vendor
   device-get-id-bustype

   device-removed@-name
   device-removed@-output-name
   device-removed@-sys-name
   device-removed@-vendor
   device-removed@-product

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


(in-package #:libinput)
(define-foreign-library libinput
  (:unix (:or
	  "libinput.so"
	  "libinput.so.10"
          "/usr/lib64/libinput.so"
          "/usr/lib64/libinput.so.10"
          "/usr/lib/x86_64-linux-gnu/libinput.so.10"))
  (t (:default "libinput")))

(use-foreign-library libinput)
