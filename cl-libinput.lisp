
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
  (list
   (cons 0 :none)
   (cons 1 :device-added)
   (cons 2 :device-removed)
   (cons 300 :keyboard-key)
   (cons 400 :pointer-motion)
   (cons 401 :pointer-motion-absolute)
   (cons 402 :pointer-button)
   (cons 403 :pointer-axis)
   (cons 404 :pointer-scroll-wheel)
   (cons 405 :pointer-scroll-finger)
   (cons 406 :pointer-scroll-continuous)
   (cons 500 :touch-down)
   (cons 501 :touch-up)
   (cons 502 :touch-motion)
   (cons 503 :touch-cancel)
   (cons 504 :touch-frame)
   (cons 600 :tablet-tool-axis)
   (cons 601 :tablet-tool-proximity)
   (cons 602 :tablet-tool-tip)
   (cons 603 :tablet-tool-button)
   (cons 700 :tablet-pad-button)
   (cons 701 :tablet-pad-ring)
   (cons 702 :tablet-pad-strip)
   (cons 703 :tablet-pad-key)
   (cons 704 :tablet-pad-dial)
   (cons 800 :gesture-swipe-begin)
   (cons 801 :gesture-swipe-update)
   (cons 802 :gesture-swipe-end)
   (cons 803 :gesture-pinch-begin)
   (cons 804 :gesture-pinch-update)
   (cons 805 :gesture-pinch-end)
   (cons 806 :gesture-hold-begin)
   (cons 807 :gesture-hold-end)
   (cons 900 :switch-toggle)))

;; TODO: Assuming that 0 means released
;; Check during run time
(defcenum libinput-key-state
  (:released 0)
  (:pressed 1))

;; TODO: Same as previous
;; NOTE: button here means - key button
(defcenum libinput-button-state
  (:released 0)
  (:pressed 1))


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

(defcfun ("libinput_event_keyboard_get_key_state" event-keyboard-get-key-state) libinput-key-state
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

(defcfun ("libinput_event_pointer_get_absolute_x" event-pointer-get-absolute-x) :double
  (pointer-event :pointer))

(defcfun ("libinput_event_pointer_get_absolute_y" event-pointer-get-absolute-y) :double
  (pointer-event :pointer))

(defcfun ("libinput_event_pointer_get_dx" event-pointer-get-dx) :double
  (pointer-event :pointer))

(defcfun ("libinput_event_pointer_get_dy" event-pointer-get-dy) :double
  (pointer-event :pointer))

;; Touch event readers
;; NOTE: Missing the usec time and transformed x/y
(defcfun ("libinput_event_touch_get_time"      event-touch-get-time)      :uint32 (event :pointer))
(defcfun ("libinput_event_touch_get_x"         event-touch-get-x)         :double (event :pointer))
(defcfun ("libinput_event_touch_get_y"         event-touch-get-y)         :double (event :pointer))
(defcfun ("libinput_event_touch_get_slot"      event-touch-get-slot)      :int32  (event :pointer))
(defcfun ("libinput_event_touch_get_seat_slot" event-touch-get-seat-slot) :int32  (event :pointer))

;; GESTURE
(defcfun ("libinput_event_gesture_get_time"         gesture-time)         :uint32 (event :pointer))
(defcfun ("libinput_event_gesture_get_finger_count" gesture-finger-count) :int    (event :pointer))
(defcfun ("libinput_event_gesture_get_dx"           gesture-dx)           :double (event :pointer))
(defcfun ("libinput_event_gesture_get_dy"           gesture-dy)           :double (event :pointer))
(defcfun ("libinput_event_gesture_get_scale"        gesture-scale)        :double (event :pointer))
(defcfun ("libinput_event_gesture_get_angle_delta"  gesture-angle-delta)  :double (event :pointer))
(defcfun ("libinput_event_gesture_get_cancelled"    gesture-cancelled)    :int    (event :pointer))

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
    (with-foreign-slots ((open-restricted close-restricted) interface (:struct libinput-interface))
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
  (let ((event (%get-event context)))
    (if (not (pointer-eq event (null-pointer)))
	(let* ((event-type (cdr (assoc (event-get-type event) *event-types*))))
	  (prog1
	      (funcall
	       (case event-type
		 (:none           'none@)
		 (:device-added   'mk-device-added@)
		 (:device-removed 'mk-device-removed@)
		 (:keyboard-key   'mk-keyboard@)
		 (:touch-up       'mk-touch-up@)
		 (:touch-down	  'mk-touch-down@)
		 (:touch-motion	  'mk-touch-motion@)
		 (:touch-cancel	  'mk-touch-cancel@)
		 (:touch-frame	  'mk-touch-frame@)
		 (:pointer-button 'mk-pointer-button@)

		 (:gesture-hold-begin   'mk-gesture-hold-begin@)
		 (:gesture-hold-end     'mk-gesture-hold-end@)
		 (:gesture-swipe-begin  'mk-gesture-swipe-begin@)
		 (:gesture-swipe-update 'mk-gesture-swipe-update@)
		 (:gesture-swipe-end    'mk-gesture-swipe-end@)
		 (:gesture-pinch-begin  'mk-gesture-pinch-begin@)
		 (:gesture-pinch-update 'mk-gesture-pinch-update@)
		 (:gesture-pinch-end    'mk-gesture-pinch-end@)

		 ((:pointer-motion :pointer-motion-absolute) 'mk-pointer-motion@)
		 (t (error "The dev writing this got lazy and didn't cover the event type ~A" event-type)))
	       event
	       event-type)
	    (event-destroy event)))
	nil)))

;; ┌─┐┬  ┬┌─┐┌┐┌┌┬┐┌─┐┬─┐┌─┐
;; ├┤ └┐┌┘├┤ │││ │ │ │├┬┘└─┐
;; └─┘ └┘ └─┘┘└┘ ┴ └─┘┴└─└─┘
;; Replacing the word -event with the symbol @
(defstruct event device type)
(defstruct (none@ (:include event)))

;; DEVICE
(defstruct (device@ (:include event)))
(defstruct (device-added@ (:include device@)))
(defstruct (device-removed@ (:include device@)))

;; KEYBOARD
(defstruct (keyboard@ (:include event))                 time key state)

;; POINTER
(defstruct (pointer-motion@ (:include event))           time dx dy)
(defstruct (pointer-button@ (:include pointer-motion@)) button state)

;; TOUCH
(defstruct (touch-frame@ (:include event)) time)
(defstruct (touch-up@ (:include touch-frame@)) slot seat-slot)
(defstruct (touch@ (:include touch-frame@)) x y slot seat-slot)
(defstruct (touch-down@ (:include touch@)))
(defstruct (touch-motion@ (:include touch@)))
(defstruct (touch-cancel@ (:include touch@)))

;; GESTURE - seems to be a touchpad
(defstruct (gesture@ (:include event)) time finger-count cancelled dx dy scale angle-delta)
(defstruct (gesture-hold-begin@ (:include gesture@)))
(defstruct (gesture-hold-end@ (:include gesture@)))
(defstruct (gesture-swipe-begin@ (:include gesture@)))
(defstruct (gesture-swipe-update@ (:include gesture@)))
(defstruct (gesture-swipe-end@ (:include gesture@)))
(defstruct (gesture-pinch-begin@ (:include gesture@)))
(defstruct (gesture-pinch-update@ (:include gesture@)))
(defstruct (gesture-pinch-end@ (:include gesture@)))


;; ┌─┐┬  ┬┌─┐┌┐┌┌┬┐┌─┐┬─┐  ┌─┐┌─┐┌┐┌┌─┐┌┬┐┬─┐┬ ┬┌─┐┌┬┐┌─┐┬─┐┌─┐
;; ├┤ └┐┌┘├┤ │││ │ │ │├┬┘  │  │ ││││└─┐ │ ├┬┘│ ││   │ │ │├┬┘└─┐
;; └─┘ └┘ └─┘┘└┘ ┴ └─┘┴└─  └─┘└─┘┘└┘└─┘ ┴ ┴└─└─┘└─┘ ┴ └─┘┴└─└─┘
(defun mk-none@ (event type) (make-none@ :device event :type type))

;; DEVICE
(defun mk-device-added@ (event type) (make-device-added@ :device event :type type))
(defun mk-device-removed@ (event type) (make-device-removed@ :device event :type type))

;; KEYBOARD
(defun mk-keyboard@ (event type)
  (make-keyboard@
   :type type
   :device (event-get-device event)
   :time (event-keyboard-get-time event)
   :key (event-keyboard-get-key event)
   :state (event-keyboard-get-key-state event)))

;; POINTER
(defun mk-pointer-motion@ (event type)
  (make-pointer-motion@
   :type   type
   :device (event-get-device event)
   :time   (event-pointer-get-time event)
   :dx     (event-pointer-get-dx event)
   :dy     (event-pointer-get-dy event)))

(defun mk-pointer-button@ (event type)
  (make-pointer-button@
   :type type
   :device (event-get-device event)
   :time (event-pointer-get-time event)
   :button (event-pointer-get-button event)
   :state (event-pointer-get-button-state event)))

;; TOUCH
(defun touch-properties (event type)
  (list :x (event-touch-get-x event) :y (event-touch-get-y event)
	:time (event-touch-get-time event) :type type
	:slot (event-touch-get-slot event)
	:seat-slot (event-touch-get-seat-slot event)
	:device (event-get-device event)))

(defun mk-touch-down@   (event type) (apply 'make-touch-down@   (touch-properties event type)))
(defun mk-touch-motion@ (event type) (apply 'make-touch-motion@ (touch-properties event type)))
(defun mk-touch-cancel@ (event type) (apply 'make-touch-cancel@ (touch-properties event type)))
(defun mk-touch-frame@ (event type)
  (make-touch-frame@
   :type type
   :device (event-get-device event)
   :time (event-touch-get-time event)))

(defun mk-touch-up@ (event type)
  (make-touch-up@
   :type type
   :device (event-get-device event)
   :time (event-touch-get-time event)
   :slot (event-touch-get-slot event)
   :seat-slot (event-touch-get-seat-slot event)))

;; GESTURE
(defun gesture-properties (event type)
  (list :time (gesture-time event) :type type :device (event-get-device event)
	:dx (gesture-dx event) :dy (gesture-dy event)
	:scale (gesture-scale event) :angle-delta (gesture-angle-delta event)
	:cancelled (gesture-cancelled event) :finger-count (gesture-finger-count event)))

(defun mk-gesture-hold-begin@ (event type) (apply 'make-gesture-hold-begin@ (gesture-properties event type)))
(defun mk-gesture-hold-end@ (event type) (apply 'make-gesture-hold-end@ (gesture-properties event type)))
(defun mk-gesture-swipe-begin@ (event type) (apply 'make-gesture-swipe-begin@ (gesture-properties event type)))
(defun mk-gesture-swipe-update@ (event type) (apply 'make-gesture-swipe-update@ (gesture-properties event type)))
(defun mk-gesture-swipe-end@ (event type) (apply 'make-gesture-swipe-end@ (gesture-properties event type)))
(defun mk-gesture-pinch-begin@ (event type) (apply 'make-gesture-pinch-begin@ (gesture-properties event type)))
(defun mk-gesture-pinch-update@ (event type) (apply 'make-gesture-pinch-update@ (gesture-properties event type)))
(defun mk-gesture-pinch-end@ (event type) (apply 'make-gesture-pinch-end@ (gesture-properties event type)))
