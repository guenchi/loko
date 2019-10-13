# Drivers in Loko Scheme

## Basic concepts

A driver is a piece of code that allows for abstract access to a
hardware device. In Loko Scheme they are collected in the `drivers`
directory.

### Driver abstractions

A large part of creating drivers is to adapt the hardware to the
abstractions used in the rest of the system. Sometimes this means that
the full capabilities of the hardware are hidden. A serial port
supported by a UART may appear as a Scheme input port and an output
port, which will allow you to hook it up to any Scheme code that works
with ports. But a UART can do much more than a Scheme port can. An
output port can't usually control baud rates or send breaks.

Scheme lacks abstractions for hardware. In the RnRS standards, files
and ports are the only things that work as abstractions for hardware.
This is not so bad, because Unix systems have shown that a lot of
hardware can be represented as special files (`/dev`) and the file
descriptors you get when opening the special files.

However, these file descriptors are just handles that let user space
communicate with the driver. Sometimes the communication is through an
abstraction layer and sometimes it goes almost directly to the driver.
User space needs to use special syscalls (e.g. `ioctl`) to actually do
anything interesting that is not a read/write operation.

Drivers for Loko should not be locked in to any particular way of
designing the user space interface. Decisions like that are taken on a
different level. This will let developers experiment with different
user space designs. Designs
like [Barrelfish](http://www.barrelfish.org), where hardware
virtualized devices are handed out directly to user space, should be
possible to express.

Drivers in Loko should be written as libraries that provide convenient
APIs. These APIs should provide basic functionality in a way that is
common between similar hardware of the same type. The interfaces will
need to be developed over time.

When it is necessary to have concurrency (the most common case for
modern devices), drivers should use *channels* to communicate with the
rest of the system. Concurrent drivers can start as many fibers as
they like. The messages sent on these channels should preferably be
simple objects like vectors, symbols, pairs and fixnums. The messages
become part of the driver API.

### Hardware access

Drivers need access to their devices. The way this is done depends on
what type of bus the device is attached to.

Modern PCs have busses that can be probed, like PCI and USB. This
process provides enough information that you can easily detect the
type of devices that are on the bus and how to access them. Hardware
tends to appear like a tree-like structure, so it is natural that a
bus driver will pass along a reference to the bus down in the call
stack.

Older devices on the PC do not appear on the PCI bus and should be
detected and started by just knowing that they ought to be there
because it's a PC. Most ARM systems do not come with a PCI bus and
have all their devices on addresses that need to be known ahead of
time, but are different between platforms. A popular solution is to
use DeviceTree to encode this information and that is certainly
something that should be explored for Loko.

The major hardware interaction points are:

1. Scanning and configuring the bus; detecting new and removed
   devices.
2. Setting up access to the device.
3. Interfacing with the device through its registers, channels, etc.
4. Allowing the device to write to system memory.
5. Waiting on interrupts from the device.

The way these things are done depends on the bus. Further
documentation is needed. For now, please consult the source code or
ask.

## Future directions for drivers

There is an interesting thing that can be done with drivers for PCI
devices when `eval` uses online compilation. PCI devices can appear
anywhere in memory and sometimes even anywhere in I/O space. Register
access can look like this:

```scheme
(define (driver·pci·uhci dev controller)
  ;; The UHCI registers are mapped to the location in BAR4
  (let ((bar (vector-ref (pcidev-BARs dev) 4)))
    ;; Disable keyboard and mouse legacy support
    (pci-put-u16 dev #xC0 #x0000)
    (driver·uhci (if (pcibar-i/o? bar) 'i/o 'mem)
                 (pcibar-base bar)
                 (pcibar-size bar)
                 (pcidev-irq dev)
                 controller)))

(define (driver·uhci reg-type reg-base reg-size irq controller)
  ;; Access to the device registers (independent of i/o vs mem)
  (define (reg-u8-ref offset)
    (assert (fx<? -1 offset reg-size))
    (case reg-type
      ((i/o) (get-i/o-u8 (fx+ reg-base offset)))
      ((mem) (get-mem-u8 (fx+ reg-base offset)))
      (else (assert #f))))
  ...)

```

It would be interesting if `driver·pci·uhci` used `eval` to compile a
specialized version of the driver where `reg-u8-ref` (etc.) have been
inlined by cp0. After compilation, each `reg-u8-ref` call would be a
single instruction. Specializing, compiling and starting the driver
can be as simple as this:

```scheme
(let ((driver·uhci
       (eval `(lambda (controller)
                (driver-source·uhci ,reg-type ,reg-base
                                    ,reg-size ,irq
                                    controller))
             (apply environment driver-environment·uhci))))
  (driver·uhci controller))
```

In principle this kind of code would work even today, but the driver
would be slowed down because `eval` is slow.

The same principle can be applied to embedded systems that use
DeviceTree. If a static DeviceTree is used then this could even be
done as part of the build process. If a dynamic DeviceTree is used (to
allow the same kernel to run on different ARM platforms) then boot
time may become an issue. But then drivers could be designed to
initially use a non-specialized driver, call `eval` asynchronously,
and tell the running driver to switch to the specialized driver when
`eval` has returned.

