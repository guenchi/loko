# lspci for Loko Scheme

This is a sample program that lists the devices on the PCI bus.

Type `make run` to build and run it. It needs QEMU with KVM support
and the PCI IDs database.

Example output:

```
Scanning the PCI bus...

0:0.0	8086:1237  Host bridge
 Vendor: Intel Corporation
 Device: 440FX - 82441FX PMC [Natoma]
 Subsystem: Qemu virtual machine

0:1.0	8086:7000  ISA bridge
 Vendor: Intel Corporation
 Device: 82371SB PIIX3 ISA [Natoma/Triton II]
 Subsystem: Qemu virtual machine

0:1.1	8086:7010  IDE interface
 Vendor: Intel Corporation
 Device: 82371SB PIIX3 IDE [Natoma/Triton II]
 Subsystem: Qemu virtual machine
 Programming interface: ISA Compatibility mode-only controller, supports bus mastering
 BAR4 i/o #xC180 #x+10

0:1.2	8086:7020  USB controller
 Vendor: Intel Corporation
 Device: 82371SB PIIX3 USB [Natoma/Triton II]
 Subsystem: QEMU Virtual Machine
 Programming interface: UHCI
 IRQ 11
 BAR4 i/o #xC140 #x+20

0:1.3	8086:7113  Bridge
 Vendor: Intel Corporation
 Device: 82371AB/EB/MB PIIX4 ACPI
 Subsystem: Qemu virtual machine
 IRQ 9

0:2.0	1234:1111  VGA compatible controller
 Subsystem: 1AF4:1100
 Programming interface: VGA controller
 BAR0 m32 #xFD000000 #x+1000000
 BAR2 m32 #xFEBF4000 #x+1000
 ROM size #x10000

…
```
