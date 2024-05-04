;;;; cl-libinput.lisp

(in-package #:libinput)

;;; "cl-libinput" goes here. Hacks and glory await!

(define-foreign-library libinput
  (:unix (:or
	  "libinput.so"
	  "libinput.so.10"
          "/usr/lib64/libinput.so"
          "/usr/lib64/libinput.so.10"
          "/usr/lib/x86_64-linux-gnu/libinput.so.10"))
  (t (:default "libinput")))

(use-foreign-library libinput)


;; ┌─┐┌─┐┌─┐┌─┐┌┐ ┬┬  ┬┌┬┐┬┌─┐┌─┐
;; │  ├─┤├─┘├─┤├┴┐││  │ │ │├┤ └─┐
;; └─┘┴ ┴┴  ┴ ┴└─┘┴┴─┘┴ ┴ ┴└─┘└─┘
(defparameter device-cap-keyboard 0)
(defparameter device-cap-pointer 1)
(defparameter device-cap-touch 2)
(defparameter device-cap-tablet-tool 3)
(defparameter device-cap-tablet-pad 4)
(defparameter device-cap-gesture 5)
(defparameter device-cap-switch 6)


;; ┌─┐┬  ┬┌─┐┌┐┌┌┬┐┌─┐
;; ├┤ └┐┌┘├┤ │││ │ └─┐
;; └─┘ └┘ └─┘┘└┘ ┴ └─┘
(defvar *event-types*
  '(0 :none
    1 :device-added
    2 :device-removed
    300 :keyboard-key
    400 :pointer-motion
    401 :pointer-motion-absolute
    402 :pointer-button
    403 :pointer-axis
    404 :pointer-scroll-wheel
    405 :pointer-scroll-finger
    406 :pointer-scroll-continuous
    500 :touch-down
    501 :touch-up
    502 :touch-motion
    503 :touch-cancel
    504 :touch-frame
    600 :tablet-tool-axis
    601 :tablet-tool-proximity
    602 :tablet-tool-tip
    603 :tablet-tool-button
    700 :tablet-pad-button
    701 :tablet-pad-ring
    702 :tablet-pad-strip
    703 :tablet-pad-key
    704 :tablet-pad-dial
    800 :gesture-swipe-begin
    801 :gesture-swipe-update
    802 :gesture-swipe-end
    803 :gesture-pinch-begin
    804 :gesture-pinch-update
    805 :gesture-pinch-end
    806 :gesture-hold-begin
    807 :gesture-hold-end
    900 :switch-toggle))


;; ┌─┐┌┬┐┬─┐┬ ┬┌─┐┌┬┐┌─┐
;; └─┐ │ ├┬┘│ ││   │ └─┐
;; └─┘ ┴ ┴└─└─┘└─┘ ┴ └─┘
(defcstruct libinput-interface
  (open-restricted :pointer)
  (close-restricted :pointer))


;; ┌─┐┬ ┬┌┐┌┌─┐┌┬┐┬┌─┐┌┐┌┌─┐
;; ├┤ │ │││││   │ ││ ││││└─┐
;; └  └─┘┘└┘└─┘ ┴ ┴└─┘┘└┘└─┘
(defcfun ("libinput_unref" unref) :pointer
  (context :pointer))

(defcfun ("libinput_path_create_context" path-create-context) :pointer
  (interface :pointer)
  (user-date :pointer))

(defcfun ("libinput_path_add_device" path-add-device) :pointer
  (context :pointer)
  (path :string))

(defcfun ("libinput_path_remove_device" path-remove-device) :void
  (device :pointer))

(defcfun ("libinput_get_fd" get-fd) :int
  (context :pointer))

(defcfun ("libinput_dispatch" dispatch) :int
  (context :pointer))

(defcfun ("libinput_device_has_capability" device-has-capability) :int
  (device :pointer)
  (capability :int))

(defcfun ("libinput_device_get_name" device-get-name) :string
  (device :pointer))

(defcfun ("libinput_device_ref" device-ref) :pointer
  (device :pointer))

(defcfun ("libinput_device_unref" device-unref) :pointer
  (device :pointer))

(defcfun ("libinput_get_event" get-event) :pointer
  (context :pointer))

(defcfun ("libinput_event_get_type" event-get-type) :int
  (event :pointer))

(defcfun ("libinput_event_destroy" event-destroy) :void
  (event :pointer))

(defcfun ("libinput_event_get_device" event-get-device) :pointer
  (event :pointer))

(defcfun ("libinput_event_get_keyboard_event" event-get-keyboard-event) :pointer
  (event :pointer))

(defcfun ("libinput_event_keyboard_get_time" event-keyboard-get-time) :uint32
  (keyboard-event :pointer))

(defcfun ("libinput_event_keyboard_get_key" event-keyboard-get-key) :uint32
  (keyboard-event :pointer))

(defcfun ("libinput_event_keyboard_get_key_state" event-keyboard-get-key-state) :int
  (keyboard-event :pointer))

(defcfun ("libinput_event_get_pointer_event" event-get-pointer-event) :pointer
  (event :pointer))

(defcfun ("libinput_event_pointer_get_time" event-pointer-get-time) :uint32
  (pointer-event :pointer))

(defcfun ("libinput_event_pointer_get_button" event-pointer-get-button) :uint32
  (pointer-event :pointer))

(defcfun ("libinput_event_pointer_get_button_state" event-pointer-get-button-state) :int
  (pointer-event :pointer))

(defcfun ("libinput_event_pointer_get_dx" event-pointer-get-dx) :double
  (pointer-event :pointer))

(defcfun ("libinput_event_pointer_get_dy" event-pointer-get-dy) :double
  (pointer-event :pointer))


;; ┌─┐┌─┐┬  ┬  ┌┐ ┌─┐┌─┐┬┌─┌─┐
;; │  ├─┤│  │  ├┴┐├─┤│  ├┴┐└─┐
;; └─┘┴ ┴┴─┘┴─┘└─┘┴ ┴└─┘┴ ┴└─┘
(defcallback open-restricted :int ((path :string) (flags :int) (user-data :pointer))
  (let* ((context user-data) (fd (nix:open path flags)))
    (when (< fd 0) (error "Failed to open ~A" path)) fd))

(defcallback close-restricted :void ((fd :int) (user-data :pointer))
  (nix:close fd))

(defun make-libinput-interface ()
  (let ((interface (foreign-alloc '(:struct libinput-interface))))
    (setf (foreign-slot-value interface '(:struct libinput-interface) 'open-restricted)
	  (callback open-restricted))
    (setf (foreign-slot-value interface '(:struct libinput-interface) 'close-restricted)
	  (callback close-restricted))
    interface))

(defun create-context () (path-create-context (make-libinput-interface) (null-pointer)))

(defun make-keyboard-event)

(defun read-event (context)
  (let* ((event (get-event context))
	 (event-type (get (event-get-type event) *event-types*))
	 (event-device (event-get-device event)))
    (funcall
     (case event-type
       (:none nil)
       (:keyboard-key                              'mk-keyboard@)
       (:pointer-button                            'mk-pointer-button@)
       ((:pointer-motion :pointer-motion-absolute) 'mk-pointer-motion@)
       (t (error "The dev writing this got lazy and didn't cover the event type ~A" event-type)))
     event
     event-device)))


;; ┌─┐┬  ┬┌─┐┌┐┌┌┬┐┌─┐┬─┐┌─┐
;; ├┤ └┐┌┘├┤ │││ │ │ │├┬┘└─┐
;; └─┘ └┘ └─┘┘└┘ ┴ └─┘┴└─└─┘
;; Replacing the word -event with the symbol @
(defstruct event device)
(defstruct (keyboard@ (:include event))                 time key state)
(defstruct (pointer-motion@ (:include event))           time dx dy)
(defstruct (pointer-button@ (:include pointer-motion@)) button state)


;; ┌─┐┬  ┬┌─┐┌┐┌┌┬┐┌─┐┬─┐  ┌─┐┌─┐┌┐┌┌─┐┌┬┐┬─┐┬ ┬┌─┐┌┬┐┌─┐┬─┐┌─┐
;; ├┤ └┐┌┘├┤ │││ │ │ │├┬┘  │  │ ││││└─┐ │ ├┬┘│ ││   │ │ │├┬┘└─┐
;; └─┘ └┘ └─┘┘└┘ ┴ └─┘┴└─  └─┘└─┘┘└┘└─┘ ┴ ┴└─└─┘└─┘ ┴ └─┘┴└─└─┘
(defun mk-keyboard@ (event device)
  (make-keyboard@
   :device device
   :time (event-keyboard-get-time event)
   :key (event-keyboard-get-key event)
   :state (event-keyboard-get-key-state event)))

(defun mk-pointer-motion@ (event device)
  (make-pointer-motion@
   :device device
   :time (event-pointer-get-time event)
   :dx (event-pointer-get-dx event)
   :dy (event-pointer-get-dy event)))

(defun mk-pointer-button@ (event device)
  (make-pointer-button@
   :device device
   :time (event-pointer-get-time event)
   :button (event-pointer-get-button event)
   :state (event-pointer-get-button-state event)))
