
;;  ██████╗██╗      ██╗     ██╗██████╗ ██╗███╗   ██╗██████╗ ██╗   ██╗████████╗
;; ██╔════╝██║      ██║     ██║██╔══██╗██║████╗  ██║██╔══██╗██║   ██║╚══██╔══╝
;; ██║     ██║█████╗██║     ██║██████╔╝██║██╔██╗ ██║██████╔╝██║   ██║   ██║
;; ██║     ██║╚════╝██║     ██║██╔══██╗██║██║╚██╗██║██╔═══╝ ██║   ██║   ██║
;; ╚██████╗███████╗ ███████╗██║██████╔╝██║██║ ╚████║██║     ╚██████╔╝   ██║
;;  ╚═════╝╚══════╝ ╚══════╝╚═╝╚═════╝ ╚═╝╚═╝  ╚═══╝╚═╝      ╚═════╝    ╚═╝
;; "cl-libinput" goes here. Hacks and glory await!
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

(defcfun ("libinput_get_event" %get-event) :pointer
  (context :pointer))

(defcfun ("libinput_event_get_type" event-get-type) :int
  (event :pointer))

(defcfun ("libinput_event_destroy" event-destroy) :void
  (event :pointer))

(defcfun ("libinput_event_get_device" event-get-device) :pointer
  (event :pointer))

;; Keyboard event readers
(defcfun ("libinput_event_get_keyboard_event" event-get-keyboard-event) :pointer
  (event :pointer))

(defcfun ("libinput_event_keyboard_get_time" event-keyboard-get-time) :uint32
  (keyboard-event :pointer))

(defcfun ("libinput_event_keyboard_get_key" event-keyboard-get-key) :uint32
  (keyboard-event :pointer))

(defcfun ("libinput_event_keyboard_get_key_state" event-keyboard-get-key-state) :int
  (keyboard-event :pointer))

;; Pointer event readers
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

;; Touch event readers
;; NOTE: Missing the usec time and transformed x/y
(defcfun ("libinput_event_touch_get_time" event-touch-get-time) :uint32
  (event :pointer))

(defcfun ("libinput_event_touch_get_x" event-touch-get-x) :double
  (event :pointer))

(defcfun ("libinput_event_touch_get_y" event-touch-get-y) :double
  (event :pointer))


;; ┌─┐┌─┐┬  ┬  ┌┐ ┌─┐┌─┐┬┌─┌─┐
;; │  ├─┤│  │  ├┴┐├─┤│  ├┴┐└─┐
;; └─┘┴ ┴┴─┘┴─┘└─┘┴ ┴└─┘┴ ┴└─┘
(defvar *open-restricted* nil)
(defcallback open-restricted-cb :int ((path :string) (flags :int) (user-data :pointer))
  (funcall *open-restricted* path flags user-data))

(defvar *close-restricted* nil)
(defcallback close-restricted-cb :void ((fd :int) (user-data :pointer))
  (funcall *close-restricted* fd user-data))

(defun make-libinput-interface (open-restricted-lisp close-restricted-lisp)
  (setf *open-restricted* open-restricted-lisp *close-restricted* close-restricted-lisp)
  (let ((interface (foreign-alloc '(:struct libinput-interface))))
    (with-foreign-slots ((open-restricted close-restricted) interface '(:struct libinput-interface))
      (setf open-restricted (callback open-restricted-cb)
	    close-restricted (callback close-restricted-cb)))
    interface))

(defun create-context (&key open-restricted close-restricted user-data)
  "Create a new libinput context.
Two callbacks are required, defined via the keys
:open-restricted and :close-restricted.

:open-restricted is a function which takes a path, flags (c open) and a pointer to user-data
and is expected to return a valid file descriptor.

:close-restricted is a function which takes a file descriptor and a pointer to user-data

:user-data is a pointer to user data which will be passed to the callbacks,
the creation of the pointer is left up to the user.
If :user-data is not provided a null-pointer is used."

  (unless open-restricted (error "Key open-restricted (callback) is required"))
  (unless close-restricted (error "Key close-restricted (callback) is required"))
  (path-create-context (make-libinput-interface open-restricted close-restricted) (or user-data (null-pointer))))

(defun get-event (context)
  (let* ((event (%get-event context))
	 (event-type (get (event-get-type event) *event-types*)))
    (prog1
	(funcall
	 (case event-type
	   (:none nil)
	   (:keyboard-key                              'mk-keyboard@)
	   (:touch-up 				       'mk-touch-up@)
	   (:touch-down				       'mk-touch-down@)
	   (:touch-motion			       'mk-touch-motion@)
	   (:touch-cancel			       'mk-touch-cancel@)
	   (:touch-frame			       'mk-touch-frame@)
	   (:pointer-button                            'mk-pointer-button@)
	   ((:pointer-motion :pointer-motion-absolute) 'mk-pointer-motion@)
	   (t (error "The dev writing this got lazy and didn't cover the event type ~A" event-type)))
	 event
	 event-type)
      (event-destroy event))))

;; ┌─┐┬  ┬┌─┐┌┐┌┌┬┐┌─┐┬─┐┌─┐
;; ├┤ └┐┌┘├┤ │││ │ │ │├┬┘└─┐
;; └─┘ └┘ └─┘┘└┘ ┴ └─┘┴└─└─┘
;; Replacing the word -event with the symbol @
(defstruct event device evt-type)
(defstruct (keyboard@ (:include event))                 time key state)
(defstruct (pointer-motion@ (:include event))           time dx dy)
(defstruct (pointer-button@ (:include pointer-motion@)) button state)
(defstruct (touch@ (:include event))                    time x y)
(defstruct (touch-up@ (:include touch@)))
(defstruct (touch-down@ (:include touch@)))
(defstruct (touch-motion@ (:include touch@)))
(defstruct (touch-cancel@ (:include touch@)))
(defstruct (touch-frame@ (:include event)) time)


;; ┌─┐┬  ┬┌─┐┌┐┌┌┬┐┌─┐┬─┐  ┌─┐┌─┐┌┐┌┌─┐┌┬┐┬─┐┬ ┬┌─┐┌┬┐┌─┐┬─┐┌─┐
;; ├┤ └┐┌┘├┤ │││ │ │ │├┬┘  │  │ ││││└─┐ │ ├┬┘│ ││   │ │ │├┬┘└─┐
;; └─┘ └┘ └─┘┘└┘ ┴ └─┘┴└─  └─┘└─┘┘└┘└─┘ ┴ ┴└─└─┘└─┘ ┴ └─┘┴└─└─┘
(defun mk-keyboard@ (event evt-type)
  (make-keyboard@
   :evt-type evt-type
   :device (event-get-device event)
   :time (event-keyboard-get-time event)
   :key (event-keyboard-get-key event)
   :state (event-keyboard-get-key-state event)))

(defun mk-pointer-motion@ (event evt-type)
  (make-pointer-motion@
   :evt-type evt-type
   :device (event-get-device event)
   :time (event-pointer-get-time event)
   :dx (event-pointer-get-dx event)
   :dy (event-pointer-get-dy event)))

(defun mk-pointer-button@ (event evt-type)
  (make-pointer-button@
   :evt-type evt-type
   :device (event-get-device event)
   :time (event-pointer-get-time event)
   :button (event-pointer-get-button event)
   :state (event-pointer-get-button-state event)))

(defun touch-properties (event evt-type)
  (list :x (event-touch-get-x event) :y (event-touch-get-y event)
	:time (event-touch-get-time event) :evt-type evt-type
	:device (event-get-device event)))

(defun mk-touch-up@     (event evt-type) (apply 'make-touch-up@     (touch-properties event evt-type)))
(defun mk-touch-down@   (event evt-type) (apply 'make-touch-down@   (touch-properties event evt-type)))
(defun mk-touch-motion@ (event evt-type) (apply 'make-touch-motion@ (touch-properties event evt-type)))
(defun mk-touch-cancel@ (event evt-type) (apply 'make-touch-cancel@ (touch-properties event evt-type)))
(defun mk-touch-frame@ (event evt-type)
  (make-touch-frame@
   :evt-type evt-type
   :device (event-get-device event)
   :time (event-touch-get-time event)))
