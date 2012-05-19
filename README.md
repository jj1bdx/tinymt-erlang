# tinymt-erlang: Tiny Mersenne Twister (TinyMT) for Erlang

* Release date: 19-MAY-2012
* Edited and written by Kenji Rikitake (Kyoto University)
* Email contact: <kenji.rikitake@acm.org>

Copyright (c) 2012 Kenji Rikitake and Kyoto University. All rights
reserved.

Copyright (c) 2011-2012 Mutsuo Saito, Makoto Matsumoto, Hiroshima
University, and the University of Tokyo. All rights reserved.

See LICENSE.txt for the license (new/simplified BSD license). (Note:
license of rebar is Apache 2.0 License.)

This software is based on
Tiny Mersenne Twister (TinyMT) Version 1.0.1
by Mutsuo Saito (Hiroshima University) and Makoto Matsumoto (The University of Tokyo).

## Details of TinyMT algorithm

* See <http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/TINYMT/index.html>
* Also see <https://github.com/jj1bdx/TinyMT/> for my fork of the reference code

## Tested platforms

* FreeBSD/amd64 9.0-STABLE with Erlang/OTP R15B01
* FreeBSD/i386 8.3-RELEASE with Erlang/OTP R15B01
* Windows 7 64bit with Erlang/OTP R15B01 (no rebar support)
* Ubuntu Linux 12.04 with Erlang/OTP R15B01

## Building 

* Use BSD/GNU make and then

    make

The build script is Basho's rebar at <https://github.com/basho/rebar>,
which will be automatically fetched under the directory `support/`.

## Unit testing

* Use BSD/GNU make and then

    make eunit

## Preliminary benchmark results

* On i386: tinymt takes x2 CPU time than random (slower than random) 
* On amd64: tinymt takes x0.7 CPU time than random (faster than random)

## TODO

* More documentation and code
* More evaluation and refactoring

## Code authors:

* Kenji Rikitake
* Mutsuo Saito
* Makoto Matsumoto

## THANKS to:

* Dave "dizzyd" Smith
* Tuncer Ayaz

