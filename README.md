# tinymt-erlang: Tiny Mersenne Twister (TinyMT) for Erlang

* Release date: 12-APR-2014
* Edited and written by Kenji Rikitake (k2r.org, formerly Kyoto University)
* Email contact: <kenji.rikitake@acm.org>

Copyright (c) 2012-2014 Kenji Rikitake and Kyoto University. All rights
reserved.

Copyright (c) 2011-2012 Mutsuo Saito, Makoto Matsumoto, Hiroshima
University, and the University of Tokyo. All rights reserved.

See the file `LICENSE` for the license (new/simplified BSD license). (Note:
license of rebar is Apache 2.0 License.)

This software is based on
Tiny Mersenne Twister (TinyMT) Version 1.0.1
by Mutsuo Saito (Hiroshima University) and Makoto Matsumoto (The University of Tokyo).

## Details of TinyMT algorithm

* See <http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/TINYMT/index.html>
* Also see <https://github.com/jj1bdx/TinyMT/> for my fork of the reference code

## Note: no NIF support, HiPE can be enabled

* Old NIF-supported version tagged as `20140409`
* NIF support removed
* HiPE configuration auto-detected in `rebar.config.script`
* Type specs updated for 17.0 (notably `array()` to `array:array()`)

## Notes

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

* FreeBSD/amd64 10-STABLE with Erlang/OTP 17.0
* OS X 10.9.2 with Erlang/OTP 17.0

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
* Ryosuke "Voluntas" Nakai

## Acknowledgments

* Kenji Rikitake used the supercomputer service provided by Academic
  Center for Computing and Media Studies (ACCMS), Kyoto University, for
  testing this program and fundamental studies of TinyMT generation
  parameters.

* Kenji Rikitake thanks the Program Committee of ACM Erlang Workshop
  2012 to give him a chance to present this work as a Practice and
  Application paper.
