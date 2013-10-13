# _epv_, a web-based photo gallery

## Overview

_epv_ is a minimalistic photo gallery with Web interface.
It does not need write access to directories with your media files
(all epv-related files, e.g., user defined meta info, thumbnails
and resized pictures, are kept in a separate location).

## Requirements

* erlang;
* erlang-inets;
* imagemagick;
* ffmpeg (for video support).

To build HTML documentation, you need erlang-edoc.

## Building

To build all Erlang binaries as well as HTML documentation, execute:

    $ make

If all you need is just binaries, do:

    $ make compile

## Running

The easiest way to start it:

    $ make epv
    $ ./epv /path/to/your/media /path/where/epv/thumbs/will/be/stored

## Installing

Sorry, there are no RPM or DEB packages yet, but you can
install _epv_ as a separate service in Debian with:

    $ sudo make debian-install

which will put an epv start/stop script in /etc/init.d.
After starting _epv_ with something like "service epv start", _epv_
will start serving http requests on port 8080 (default, configurable).

There is a few targets for staged installation (using DESTDIR):

* install - installs Erlang application itself;
* install-doc - installs this memo and license;
* install-html - installs generated HTML documentation.

## Configuration

TCP port and other settings for _epv_ are stored in /etc/epv.config. By default,
logs will be created in /var/log/epv, and thumbnails and resized images will be stored in
/var/lib/epv/meta.

## Adding some media

Simply add new directories or image files (or symlink them) to
the so called "media dir" (defined in configuration). No restart or
extra actions required except F5 in your browser.

-----------------------------------------------------------------
Aleksey Morarash <aleksey.morarash@gmail.com>, 2012

