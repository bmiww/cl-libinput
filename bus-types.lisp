
;; ██████╗ ██╗   ██╗███████╗   ████████╗██╗   ██╗██████╗ ███████╗███████╗
;; ██╔══██╗██║   ██║██╔════╝   ╚══██╔══╝╚██╗ ██╔╝██╔══██╗██╔════╝██╔════╝
;; ██████╔╝██║   ██║███████╗█████╗██║    ╚████╔╝ ██████╔╝█████╗  ███████╗
;; ██╔══██╗██║   ██║╚════██║╚════╝██║     ╚██╔╝  ██╔═══╝ ██╔══╝  ╚════██║
;; ██████╔╝╚██████╔╝███████║      ██║      ██║   ██║     ███████╗███████║
;; ╚═════╝  ╚═════╝ ╚══════╝      ╚═╝      ╚═╝   ╚═╝     ╚══════╝╚══════╝
;; NOTE: Taken from linux headers
;; https://github.com/torvalds/linux/blob/master/include/uapi/linux/input.h
(in-package #:libinput)

(defcenum (bus-type :uint)
  (:pci #x01)
  (:isapnp #x02)
  (:usb #x03)
  (:hil #x04)
  (:bluetooth #x05)
  (:virtual #x06)
  (:isa #x10)
  (:i8042 #x11)
  (:xtkbd #x12)
  (:rs232 #x13)
  (:gameport #x14)
  (:parport #x15)
  (:amiga #x16)
  (:adb #x17)
  (:i2c #x18)
  (:host #x19)
  (:gsc #x1A)
  (:atari #x1B)
  (:spi #x1C)
  (:rmi #x1D)
  (:cec #x1E)
  (:intel-ishtp #x1F)
  (:amd-sfh #x20))
