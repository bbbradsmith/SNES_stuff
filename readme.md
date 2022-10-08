# SNES Stuff

A collection of small SNES programs. Demonstrations and experiments.

Website: [rainwarrior.ca](https://rainwarrior.ca)

Patreon: [rainwarrior](https://patreon.com/rainwarrior)

## Demos

* **[ctrltest](ctrltest)** (2022-10-08)
  * SNES generic controller test
  * ROM [ctrltest.sfc](../../raw/main/ctrltest/ctrltest.sfc)
  * ROM [ctrltest_auto.sfc](../../raw/main/ctrltest/ctrltest_auto.sfc)

* **[mset](mset)** (2022-10-08)
  * SNES Mouse test
  * ROM [mset.sfc](../../raw/main/mset/mset.sfc)

* **[iplfast](iplfast)** (2022-10-04)
  * SPC-700 IPL fast loader
  * ROM [iplfast.sfc](../../raw/main/iplfast/iplfast.sfc)

* **[noise31](noise31)** (2022-09-04)
  * Minimal SPC-700 sound example, plays noise loudly
  * ROM [noise31.sfc](../../raw/main/noise31/noise31.sfc)

* **[palcycle](palcycle)** (2022-08-30)
  * Demonstration of 256-colour palette cycling
  * ROM [palcycle.sfc](../../raw/main/palcycle/palcycle.sfc)

* **[colourmath](colourmath)** (2022-08-20)
  * Demonstration of colour math uses
  * ROM [colourmath.sfc](../../raw/main/colourmath/colourmath.sfc)

* **[dizworld](dizworld)** (2022-08-17)
  * Demonstration of practical uses for Mode 7 graphics
  * ROM [dizworld.sfc](../../raw/main/dizworld/dizworld.sfc)

* **[multest](multest)** (2022-06-06)
  * Test of hardware multiply/divide algorithms
  * ROM: [multest_mul16.sfc](../../raw/main/multest/multest_mul16.sfc)
  * ROM: [multest_div16.sfc](../../raw/main/multest/multest_div16.sfc)

* **[extbgtest](extbgtest)** (2022-06-06)
  * Test of Mode 7 EXTBG
  * ROM: [extbgtest.sfc](../../raw/main/extbgtest/extbgtest.sfc)

* **[twoship](twoship)** (2022-05-19)
  * Demonstration of high resolution Mode 5 graphics
  * ROM: [twoship.sfc](../../raw/main/twoship/twoship.sfc)

* **[elasticity](elasticity)** (2022-05-05)
  * Demonstration of high colour graphics
  * ROM: [elasticity.sfc](../../raw/main/elasticity/elasticity.sfc)

## Required Tools

You can download and run the ROMs in an emulator, or with a flash cart, but rebuilding them may require some of the following tools:

* [cc65](https://cc65.github.io/)
  * C compiler and assembler for 6502/65C816.
  * Used for all projects.
  * Download "windows snapshot".
* [python 3](https://www.python.org/)
* [Pillow](https://pillow.readthedocs.io/en/stable/installation.html#basic-installation)
  * Python is programming language and script interpreter. Pillow is a library add-on for working with images.
  * Used for optional utility scripts in most projects, especially for rebuilding art data from PNG image sources.
  * Download and run python installer. Pillow is installed by running a python script (see installation instructions).
* [wla-dx](https://github.com/vhelin/wla-dx/releases)
  * Assembler for SPC-700.
  * Used for projects that have sound.
  * Download release.

I build all of these projects under the Windows operating system, each with an included simple batch file script.

The tools used are open source, and available for other operating systems,
but if this is needed you will have to rewrite the batch scripts for yourself.
