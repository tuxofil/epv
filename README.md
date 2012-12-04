# epv -- web based photo gallery

## Overview

epv is minimalistic photo gallery with Web interface.
It does not any changes to directory with your media files
and store all epv-related files (user defined meta info, thumbnails
and resized pictures) to separate location.

## Requirements

You need following packages installed to run epv:

* erlang;
* erlang-inets;
* imagemagick.

To build HTML documentation you need erlang-edoc package installed.

## Building

Chdir to epv sources top dir and type:

$ make

It will compile all Erlang binaries and HTML documentation. If
you do not want generate HTML documentation, type:

$ make compile

## Installing

Sorry, there is no {RPM,DEB} packages for now. But you can
install epv as separate service in Debian with:

$ sudo make install-debian

epv init.d-script is all you need.
After starting epv with something like "service epv start" point
your browser at http://127.0.0.1:8080/ (8080 - is the default
TCP port number).

Configuration file located at /etc/epv.config, logs will be stored
in /var/log/epv/, thumbnails and resized images will be stored in
/var/lib/epv/meta (by default).

## Adding some media

Simply add new directories or image files (or symlink them) to
so called "media dir" (defined in configuration). No restart or
extra actions required but F5 in your browser.

-----------------------------------------------------------------
              Aleksey Morarash <aleksey.morarash@gmail.com>, 2012

