# tinymt-erlang: Tiny Mersenne Twister (TinyMT) for Erlang

* Release date: 11-JUL-2012
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

## Features

* Minimal NIF version added as `tinymt32_nif`.

## Notes

* In versions released previously before 31-MAY-2012 (inclusive),
  `seed0/0` had a wrong value in the `#status2` member of the record
  which exceeded the limit of 2^32.  This has been fixed by the
  corresponding 32-bit value.

* `tinymt32_nif.erl` and `tinymt32_nif_tests.erl` will cause compilation errors
  when building with HiPE options in `rebar.config`; move the source files
  out of the `src/` directory when testing HiPE.

## On execution speed

* The HiPE version is about two to three times faster than non-HiPE-non-NIF
  version, measured in overall execution time, on x86_64/amd64
  architectures. (see `test-scripts/testspeed.escript`) If you need a
  fast execution, compile with the HiPE option. (see `rebar.config`)

* The non-HiPE-non-NIF version is about six times slower than `random` module
  on x86_64/amd64, measured by `fprof`.  This is presumably due to increased
  computational complexity of the algorithm.

* The NIF version is about six times faster than non-HiPE-non-NIF
  version on x86_64/amd64, measured by `fprof`.  The bottleneck is
  presumably function calls and memory allocation themselves than the
  computational work for TinyMT internal state recursion.

## Tested platforms

* FreeBSD/amd64 9.0-STABLE with Erlang/OTP R15B01
* FreeBSD/i386 8.3-RELEASE with Erlang/OTP R15B01
* Windows 7 64bit with Erlang/OTP R15B01 (no rebar support)
* Ubuntu Linux 12.04 with Erlang/OTP R15B01
* RedHat Enterprise Linux 6 with Erlang/OTP R15B01

## Building 

* Use BSD/GNU make and then

    make

The build script is Basho's rebar at <https://github.com/basho/rebar>,
which will be automatically fetched under the directory `support/`.

## Unit testing

* Use BSD/GNU make and then

    make eunit

## TODO

* More documentation and code
* More evaluation and refactoring

## Code authors

* Kenji Rikitake
* Mutsuo Saito
* Makoto Matsumoto

## THANKS to

* Dave "dizzyd" Smith
* Tuncer Ayaz

## Acknowledgments

* Kenji Rikitake used the supercomputer service provided by Academic
  Center for Computing and Media Studies (ACCMS), Kyoto University, for
  testing this program and fundamental studies of TinyMT generation
  parameters.

* Kenji Rikitake thanks the Program Committee of ACM Erlang Workshop
  2012 to give him a chance to present this work as a Practice and
  Application paper.
