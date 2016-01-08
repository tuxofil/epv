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
* ffmpeg - optional (for video support);
* erlang-edoc - optional (for 'html' makefile target);
* erlang-eunit - optional (for 'eunit' and 'all-tests' makefile targets);
* zip - optional (for 'epv' makefile target).

## Building

To build all Erlang binaries as well as HTML documentation, execute:

    $ make

If all you need is just canonic Erlang binaries, do:

    $ make compile

To build HTML documentation (in doc subdir):

    $ make html

To build all-in-one-file binary (standalone escript):

    $ make epv

## Running

The easiest way to start it:

    $ make epv
    $ ./epv /path/to/your/media /path/where/epv/thumbs/will/be/stored

For further details see output of

    $ ./epv --help

## Testing

To run functional tests type:

    $ make test

To run unit tests type:

    $ make eunit

To run dialyzer tests type:

    $ make dialyze

To run all mentioned tests type:

    $ make all-tests

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

When running as normal Erlang application all configurations is taken
from Erlang application environment. Example of such configuration is
presented in app.config file.

When running as all-in-one-file escript (see 'epv' makefile target)
all configurations is set with epv command line options and arguments.
See 'epv --help' output for more details.

## Adding some media

Simply add new directories or image files (or symlink them) to
the so called "media dir" (defined in configuration). No restart or
extra actions required except F5 in your browser.

-----------------------------------------------------------------
Aleksey Morarash <aleksey.morarash@gmail.com>, 2012

