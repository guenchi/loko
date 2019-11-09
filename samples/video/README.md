# Video mode demo for Loko

There are two samples here. The first one uses the Bochs Graphics
Adapter (BGA) to enable graphics and draw to the screen.

The second one uses the VBE driver to set a high resolution mode with
a linear framebuffer. It has been verified to work on real hardware,
but it certainly has not be tested widely. The real Video BIOS also
ran quite slowly in the emulator. On one test machine it was not
possible to extract the Video BIOS.

The confirmed working Video BIOS was this one:

#[supervga-info
    VbeVersionMajor: 3
    VbeVersionMinor: 0
    OemString: "ATI ATOMBIOS"
    Capabilities: 1
    VideoModes: (256 257 259 261 263 272 273 275 276 278 279 281 282
                 269 270 288 403 405 406 435 437 438 451 453 454 307
                 309 310 339 341 342 355 357 358 289 290 291 292 323
                 325 326 371 373 374 387 389 390 467 469 470 483 485
                 486)
    TotalMemory: 16777216
    OemSoftwareRev: 2832
    OemVendorName: "(C) 1988-2005, ATI Technologies Inc. "
    OemProductName: "RV710"
    OemProductRev: "01.00"]

It also works with SeaVGABIOS.
