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
(defparameter none 0)
(defparameter device-added 1)
(defparameter device-removed 2)
(defparameter keyboard-key 300)
(defparameter pointer-motion 400)
(defparameter pointer-motion-absolute 401)
(defparameter pointer-button 402)
(defparameter pointer-axis 403)
(defparameter touch-down 500)
(defparameter touch-up 501)
(defparameter touch-motion 502)
(defparameter touch-cancel 503)
(defparameter touch-frame 504)
(defparameter tablet-tool-axis 600)
(defparameter tablet-tool-proximity 601)
(defparameter tablet-tool-tip 602)
(defparameter tablet-tool-button 603)
(defparameter tablet-pad-button 700)
(defparameter tablet-pad-ring 701)
(defparameter tablet-pad-strip 702)
(defparameter gesture-swipe-begin 800)
(defparameter gesture-swipe-update 801)
(defparameter gesture-swipe-end 802)
(defparameter gesture-pinch-begin 803)
(defparameter gesture-pinch-update 804)
(defparameter gesture-pinch-end 805)


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


;; ┌┬┐┌─┐┌─┐┬─┐┌─┐┌─┐
;; │││├─┤│  ├┬┘│ │└─┐
;; ┴ ┴┴ ┴└─┘┴└─└─┘└─┘
(defmacro with-pointer-motion ((event time dx dy) &body body)
  (let ((pointer-event (gensym "pointer-event")))
    `(let* ((,time (event-pointer-get-time ,event))
	    (,pointer-event (event-get-pointer-event ,event))
	    (,dx (event-pointer-get-dx ,pointer-event))
	    (,dy (event-pointer-get-dy ,pointer-event)))
       ,@body)))

(defmacro with-pointer-button ((event time button state) &body body)
  (let ((pointer-event (gensym "pointer-event")))
    `(let* ((,time (event-pointer-get-time ,event))
	    (,pointer-event (event-get-pointer-event ,event))
	    (,button (event-pointer-get-button ,pointer-event))
	    (,state (event-pointer-get-button-state ,pointer-event)))
       ,@body)))

(defmacro with-keyboard-key ((event time key state) &body body)
  (let ((keyboard-event (gensym "keyboard-event")))
    `(let* ((,time (event-keyboard-get-time ,event))
	    (,keyboard-event (event-get-keyboard-event ,event))
	    (,state (event-keyboard-get-key-state ,keyboard-event))
	    (,key (event-keyboard-get-key ,keyboard-event)))
       ,@body)))
