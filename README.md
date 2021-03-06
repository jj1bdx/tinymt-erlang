<!-- -*- coding: utf-8 -*- -->
# tinymt-erlang: Tiny Mersenne Twister (TinyMT) for Erlang

## This software is no longer maintained

*CAUTION*: this software is no longer maintained. *Migrate to the default rand module for OTP 18 and later, because they are maintained and using faster and better algorithms.*

## Version and credit information

* Version: 0.3.2
* Release date: 9-JUL-2017
* Edited and written by Kenji Rikitake
* Email contact: <kenji.rikitake@acm.org>

## Travis CI build status for the master branch

[![Build Status](https://travis-ci.org/jj1bdx/tinymt-erlang.svg?branch=master)](https://travis-ci.org/jj1bdx/tinymt-erlang)

## License

Copyright (c) 2012-2017 Kenji Rikitake and Kyoto University.
All rights reserved.

Copyright (c) 2011-2012 Mutsuo Saito, Makoto Matsumoto, Hiroshima
University, and the University of Tokyo. All rights reserved.

See the file `LICENSE` for the license (new/simplified BSD license). (Note:
license of rebar is Apache 2.0 License.)

## Details of TinyMT algorithm

This software is based on
Tiny Mersenne Twister (TinyMT) Version 1.0.1
by Mutsuo Saito (Hiroshima University) and Makoto Matsumoto (The University of Tokyo).
See <http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/TINYMT/index.html>.
Also see <https://github.com/jj1bdx/TinyMT/> for my fork of the reference code.

## Changes from the old version

* `make speed` now compares with `rand` module `exsplus` algorithm (from 0.3.2)
* Output `X` interval for `uniform/0` and `uniform_s/1` is now `0.0 < X < 1.0`, consistent with random and rand modules (from 0.3.0)
* Old NIF-supported version tagged as `20140409`
* Old include file can be found in version tagged as `20140409`
* NIF support removed
* Type specs updated for 17.0 (notably `array()` to `array:array()`)
* The internal state now have opaque type `tinymt32:intstate32/0`
* `tinymt32:uniform_s_list/{2,3}` are removed
* EDoc comment added: do `make doc` for generating the documentation
* New functions for changing generation parameters `tinymt32:setgenparams/{1,3}` are added
* Common Test suite added as `test/tinymt32_SUITE.erl`
* Removed EUnit code
* HiPE options can be activatied in `Makefile.tinymt`

## Notes

* Output `X` interval for `uniform/0` and `uniform_s/1` is now `0.0 < X < 1.0`, consistent with random and rand modules (from 0.3.0)
* *Test bug*: In versions released previously before 13-JAN-2014,
  `test_speed_orig_uniform_rec1/5` in `tinymt32_tests` and `tinymt32_nif_tests`
  errorneously called `test_speed_orig_uniform_n_rec1/5` (note `_n_`).
  This bug would have been affected the evaluation result of the Erlang Workshop 2012 paper.
  The re-measured loop test execution time of `random:uniform_s/1` is approx. 70 to 80 percent of
  that of `random:uniform_s/2`.  This bug was found with the dialyzer program of R16B03.

* In versions released previously before 31-MAY-2012 (inclusive),
  `seed0/0` had a wrong value in the `#status2` member of the record
  which exceeded the limit of 2^32.  This has been fixed by the
  corresponding 32-bit value.

## On execution speed

* The HiPE version is about two to three times faster than non-HiPE
  version, measured in overall execution time, on x86\_64/amd64
  architectures. (see `test-scripts/testspeed.escript`) If you need a
  fast execution, compile with the HiPE option. (see `rebar.config`)

* The non-HiPE version is about six times slower than `random` module
  on x86\_64/amd64, measured by `fprof`.  This is presumably due to increased
  computational complexity of the algorithm.

## Tested platforms

* OS X 10.12.5 with Erlang/OTP 20.0.1

## Make options (of erlang.mk)

* `Makefile` works on both BSD/GNU make
* `Makefile.tinymt` is the real GNU make file; edit this file for modification
* Building: `make`
* Documentation: `make docs`
* Testing: `make tests`
* Execution speed benchmark: `make speed`
* See also [erlang.mk](https://github.com/extend/erlang.mk) for the details

## hex.pm support

* Package name: `tinymt`
* See `mix.exs`

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
* Ryosuke "Voluntas" Nakai
* Loïc Hoguin

## Acknowledgments

* Kenji Rikitake used the supercomputer service provided by Academic
  Center for Computing and Media Studies (ACCMS), Kyoto University, for
  testing this program and fundamental studies of TinyMT generation
  parameters.

* Kenji Rikitake thanks the Program Committee of ACM Erlang Workshop
  2012 to give him a chance to present this work as a Practice and
  Application paper.
