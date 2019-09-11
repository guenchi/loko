# RTL8139 ethernet dumper demo

This demonstrates several aspects of Loko in a program that reads
Ethernet frames from an RTL8139 network card and prints them on the
VGA text mode console.

You can try it on real hardware if you have an RTL8139 lying around.
To run this in QEMU you'll need to create a tap0 network device:

```
$ sudo tunctl -t tap0 -u `whoami`
$ sudo ip a a 10.11.12.1/24 dev tap0
```

To generate some traffic, you can try to ping 10.11.12.2. The demo
doesn't respond to arp requestse, but you can manually add an entry to
get your pings to go through to the emulator:

```
sudo arp -i tap0 -s 10.11.12.2 aa:bb:cc:dd:ee:ff
```

Type `make run` to get the sample going in QEMU.
