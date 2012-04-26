%% (This is a simplified BSD license.)
%%
%% Copyright (c) 2012 Kenji Rikitake and Kyoto University. All rights
%% reserved.
%%
%% Copyright (c) 2011 Mutsuo Saito, Makoto Matsumoto, Hiroshima
%% University and The University of Tokyo. All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are
%% met:
%%
%%     * Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above
%%       copyright notice, this list of conditions and the following
%%       disclaimer in the documentation and/or other materials provided
%%       with the distribution.
%%     * Neither the name of the Hiroshima University, The University of
%%       Tokyo, Kyoto University, nor the names of its contributors may be
%%       used to endorse or promote products derived from this software
%%       without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(tinymt32).

-export([next_state/1,
	 temper/1,
	 testloop/1]).

-include("tinymt32.hrl").

-spec next_state(#intstate32{}) -> #intstate32{}.

next_state(R) ->
    Y0 = R#intstate32.status3,
    X0 = (R#intstate32.status0 band ?TINYMT32_MASK)
	bxor R#intstate32.status1 bxor R#intstate32.status2
	band ?MASK32,
    X1 = (X0 bxor (X0 bsl ?TINYMT32_SH0)) band ?MASK32,
    Y1 = (Y0 bxor (Y0 bsr ?TINYMT32_SH0) bxor X1) band ?MASK32,
    S0 = R#intstate32.status1,
    S10 = R#intstate32.status2,
    S20 = (X1 bxor (Y1 bsl ?TINYMT32_SH1)) band ?MASK32,
    S3 = Y1,
    YF = case (Y1 band 1) of
	     0 -> 0;
	     1 -> ?MASK32
	 end,
    S1 = S10 bxor (R#intstate32.mat1 band YF),
    S2 = S20 bxor (R#intstate32.mat2 band YF),
    R#intstate32{status0 = S0, status1 = S1, status2 = S2, status3 = S3}.

-spec temper(#intstate32{}) -> uint32().

temper(R) ->
    T0 = R#intstate32.status3,
    T1 = (R#intstate32.status0 + (R#intstate32.status2 bsr ?TINYMT32_SH8))
	band ?MASK32,
    T2 = T0 bxor T1,
    case (T1 band 1) of
	0 -> T2;
	1 -> T2 bxor R#intstate32.tmat
    end.

-spec testloop(pos_integer()) -> list().
		       

testloop(N) ->
    R = #intstate32{status0 = 297425621, status1 = 2108342699,
            status2 = 2289213227463, status3 = 2232209075,
            mat1 = 2406486510, mat2 = 4235788063, tmat = 932445695},
    testloop(N, R, []).

testloop(0, _, L) ->
    lists:reverse(L);
testloop(N, R, L) ->
    R1 = next_state(R),
    testloop(N - 1, R1, [temper(R1)|L]).


