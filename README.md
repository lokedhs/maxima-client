CLIM-based client for Maxima
============================

This is a work in progress, and building this application is currently
a bit harder than it should be.

Precompiled binaries
====================

Currently there are two ways to run a precompiled binary:

There is a Flatpak distribuion available on Flathub:
https://flathub.org/apps/details/com.dhsdevelopments.Climaxima

An Appimage is also available here:
https://github.com/lokedhs/docker-maxima-client/

Building
========

To build the application manually, the following dependencies must be
satisfied:

  - libfreetype development headers
  - libfontconfig development headers
  - libharfbuzz development headers
  - mupdf. Specifically, mutool. (there is code to perform image
    conversion using ghostscript instead, but it's not used by
    default)
  - Maxima built from source
  - Freetype renderer enabled for McCLIM

To install the required packages on Ubuntu or Debian, install the
following packages:

```
libfreetype6-dev libfontconfig1-dev libharfbuzz-dev
```

To enable the Freetype renderer, add the following line to
`$HOME/.sbclrc`:

```
(pushnew :mcclim-ffi-freetype *features*)
```

Build and install Maxima from source
------------------------------------

```
$ git clone https://git.code.sf.net/p/maxima/code maxima-code
$ cd maxima-code
$ ./bootstrap
$ ./configure --enable-sbcl
$ make
$ sudo make install
```

Build the documentation
-----------------------

Build helper binary:

```
$ cd infoparser
$ ./build-binary.sh

```

Parse the Maxima texinfo documentation files by loading the
`infoparser` ASDF system and call
`(infoparser:generate-doc-directory)`.


Build and run the application
-----------------------------

Load the system `maxima-client` and run
`(maxima-client:maxima-client)`.
