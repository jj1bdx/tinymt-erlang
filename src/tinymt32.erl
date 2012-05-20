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

-export([init/2,
	 init_by_list32/2,
	 next_state/1,
	 seed0/0,
	 seed/0,
	 seed/1,
	 seed/3,
	 temper/1,
	 temper_float/1,
	 uniform/0,
	 uniform/1,
	 uniform_s/1,
	 uniform_s/2,
	 testloop/1]).

-include("tinymt32.hrl").

-define(TWOPOW32, 16#100000000).

-define(MIN_LOOP, 8).
-define(PRE_LOOP, 8).
-define(LAG, 1).
-define(MID, 1).
-define(SIZE, 4).

-spec next_state(#intstate32{}) -> #intstate32{}.

next_state(R) ->
    Y0 = R#intstate32.status3,
    X0 = (R#intstate32.status0 band ?TINYMT32_MASK)
	bxor R#intstate32.status1 bxor R#intstate32.status2,
    X1 = (X0 bxor (X0 bsl ?TINYMT32_SH0)) band ?TINYMT32_UINT32,
    Y1 = Y0 bxor (Y0 bsr ?TINYMT32_SH0) bxor X1,
    S0 = R#intstate32.status1,
    S10 = R#intstate32.status2,
    S20 = (X1 bxor (Y1 bsl ?TINYMT32_SH1)) band ?TINYMT32_UINT32,
    S3 = Y1,
    Y1M = (?TWOPOW32 - (Y1 band 1)) band ?TINYMT32_UINT32,
    S1 = S10 bxor (R#intstate32.mat1 band Y1M),
    S2 = S20 bxor (R#intstate32.mat2 band Y1M),
    R#intstate32{status0 = S0, status1 = S1, status2 = S2, status3 = S3}.

-spec temper(#intstate32{}) -> uint32().

temper(R) ->
    T0 = R#intstate32.status3,
    T1 = (R#intstate32.status0 + (R#intstate32.status2 bsr ?TINYMT32_SH8))
	band ?TINYMT32_UINT32,
    T2 = T0 bxor T1,
    T1M = (?TWOPOW32 - (T1 band 1)) band ?TINYMT32_UINT32,
    T2 bxor (R#intstate32.tmat band T1M).

%% 0.0 <= result < 1.0
-spec temper_float(#intstate32{}) -> float().

temper_float(R) ->
    temper(R) * (1.0 / 4294967296.0).

-spec period_certification(#intstate32{}) -> #intstate32{}.

%% if the lower 127bits of the seed is all zero, reinitialize
period_certification(#intstate32{status0 = 0, status1 = 0, status2 = 0, status3 = 0,
				mat1 = M1, mat2 = M2, tmat = TM}) ->
    #intstate32{status0 = $T, status1 = $I, status2 = $N, status3 = $Y,
	        mat1 = M1, mat2 = M2, tmat = TM};
period_certification(#intstate32{status0 = 16#80000000, status1 = 0, status2 = 0, status3 = 0,
				mat1 = M1, mat2 = M2, tmat = TM}) ->
    #intstate32{status0 = $T, status1 = $I, status2 = $N, status3 = $Y,
	        mat1 = M1, mat2 = M2, tmat = TM};
period_certification(_R) -> _R.

-spec ini_func1(uint32()) -> uint32().

ini_func1(X) ->
    ((X bxor (X bsr 27)) * 1664525) band ?TINYMT32_UINT32.

-spec ini_func2(uint32()) -> uint32().

ini_func2(X) ->
    ((X bxor (X bsr 27)) * 1566083941) band ?TINYMT32_UINT32.

-spec init_rec1(integer(), integer(), array()) -> array().

init_rec1(I, N, ST) when I =:= N ->
    ST;
init_rec1(I, N, ST) when I < N ->
    V1 = array:get((I - 1) band 3, ST),
    ST1 = array:set(I band 3,
		    (array:get(I band 3, ST) bxor
			 (I + (1812433253 * (V1 bxor (V1 bsr 30))))
			 band ?TINYMT32_UINT32), ST),
    init_rec1(I + 1, N, ST1).

-spec init_rec2(integer(), integer(), #intstate32{}) -> #intstate32{}.

init_rec2(I, N, R) when I =:= N ->
    R;
init_rec2(I, N, R) when I < N ->
    R1 = next_state(R),
    init_rec2(I + 1, N, R1).

-spec init(#intstate32{}, uint32()) -> #intstate32{}.

init(R, S) ->
    ST = array:new(4),
    ST0 = array:set(0, S, ST),
    ST1 = array:set(1, R#intstate32.mat1, ST0),
    ST2 = array:set(2, R#intstate32.mat2, ST1),
    ST3 = array:set(3, R#intstate32.tmat, ST2),
    ST4 = init_rec1(1, ?MIN_LOOP, ST3),
    [V0, V1, V2, V3] = array:to_list(ST4),
    R1 = period_certification(R#intstate32{status0 = V0, status1 = V1,
					   status2 = V2, status3 = V3}),
    init_rec2(0, ?PRE_LOOP, R1).

-spec init_by_list32_rec1(integer(), integer(), [uint32()], array()) ->
				 {integer(), array()}.

init_by_list32_rec1(0, I, _, ST) ->
    {I, ST};
init_by_list32_rec1(K, I, [], ST) ->
    RR = ini_func1(array:get(I, ST) bxor
                  array:get((I + ?MID) rem ?SIZE, ST) bxor
                  array:get((I + ?SIZE - 1) rem ?SIZE, ST)),
    ST2 = array:set((I + ?MID) rem ?SIZE,
                   (array:get((I + ?MID) rem ?SIZE, ST) + RR) band ?TINYMT32_UINT32,
                    ST),
    RR2 = (RR + I) band ?TINYMT32_UINT32,
    ST3 = array:set((I + ?MID + ?LAG) rem ?SIZE,
                 (array:get((I + ?MID + ?LAG) rem ?SIZE, ST2) + RR2) band ?TINYMT32_UINT32,
                 ST2),
    ST4 = array:set(I, RR2, ST3),
    I2 = (I + 1) rem ?SIZE,
    init_by_list32_rec1(K - 1, I2, [], ST4);
init_by_list32_rec1(K, I, Key, ST) ->
    RR = ini_func1(array:get(I, ST) bxor
                  array:get((I + ?MID) rem ?SIZE, ST) bxor
                  array:get((I + ?SIZE - 1) rem ?SIZE, ST)),
    ST2 = array:set((I + ?MID) rem ?SIZE,
                   (array:get((I + ?MID) rem ?SIZE, ST) + RR) band ?TINYMT32_UINT32,
                    ST),
    [H|T] = Key,
    RR2 = (RR + H + I) band ?TINYMT32_UINT32,
    ST3 = array:set((I + ?MID + ?LAG) rem ?SIZE,
                 (array:get((I + ?MID + ?LAG) rem ?SIZE, ST2) + RR2) band ?TINYMT32_UINT32,
                 ST2),
    ST4 = array:set(I, RR2, ST3),
    I2 = (I + 1) rem ?SIZE,
    init_by_list32_rec1(K - 1, I2, T, ST4).

-spec init_by_list32_rec2(integer(), integer(), array()) -> array().

init_by_list32_rec2(0, _, ST) ->
    ST;
init_by_list32_rec2(K, I, ST) ->
    RR = ini_func2((array:get(I, ST) +
                  array:get((I + ?MID) rem ?SIZE, ST) +
                  array:get((I + ?SIZE - 1) rem ?SIZE, ST)) band ?TINYMT32_UINT32),
    ST2 = array:set((I + ?MID) rem ?SIZE,
                   (array:get((I + ?MID) rem ?SIZE, ST) bxor RR),
                   ST),
    RR2 = (RR - I) band ?TINYMT32_UINT32,
    ST3 = array:set((I + ?MID + ?LAG) rem ?SIZE,
                   (array:get((I + ?MID + ?LAG) rem ?SIZE, ST2) bxor RR2),
                   ST2),
    ST4 = array:set(I, RR2, ST3),
    I2 = (I + 1) rem ?SIZE,
    init_by_list32_rec2(K - 1, I2, ST4).

%% @doc generates an internal state from a list of 32-bit integers

-spec init_by_list32(#intstate32{}, [uint32()]) -> #intstate32{}.

init_by_list32(R, K) ->
    KL = length(K),
    ST = array:new(4),
    ST0 = array:set(0, 0, ST),
    ST1 = array:set(1, R#intstate32.mat1, ST0),
    ST2 = array:set(2, R#intstate32.mat2, ST1),
    ST3 = array:set(3, R#intstate32.tmat, ST2),
    C =
        if
            KL + 1 > ?MIN_LOOP ->
                KL + 1;
            true ->
                ?MIN_LOOP
        end,
    RR1 = ini_func1(array:get(0, ST3) bxor
                  array:get(?MID rem ?SIZE, ST3) bxor
                  array:get((?SIZE - 1) rem ?SIZE, ST3)),
    ST4 = array:set(?MID rem ?SIZE,
		    (array:get(?MID rem ?SIZE, ST3) + RR1) band ?TINYMT32_UINT32,
                    ST3),
    RR2 = (RR1 + KL) band ?TINYMT32_UINT32,
    ST5 = array:set((?MID + ?LAG) rem ?SIZE,
                   (array:get((?MID + ?LAG) rem ?SIZE, ST4) + RR2) band ?TINYMT32_UINT32,
                    ST4),
    ST6 = array:set(0, RR2, ST5),
    C1 = C - 1,
    {I1, ST7} = init_by_list32_rec1(C1, 1, K, ST6),
    ST8 = init_by_list32_rec2(?SIZE, I1, ST7),
    [V0, V1, V2, V3] = array:to_list(ST8),
    R1 = period_certification(R#intstate32{status0 = V0, status1 = V1,
					   status2 = V2, status3 = V3}),
    init_rec2(0, ?PRE_LOOP, R1).

-spec seed0() -> #intstate32{}.
		   
seed0() ->
    #intstate32{status0 = 297425621, status1 = 2108342699,
		  status2 = 2289213227463, status3 = 2232209075,
		  mat1 = 2406486510, mat2 = 4235788063, tmat = 932445695}.

-spec seed() -> #intstate32{}.
		 
seed() ->
    case seed_put(seed0()) of
        undefined -> seed0();
	#intstate32{status0 = _S0, status1 = _S1,
		    status2 = _S2, status3 = _S3,
		    mat1 = _M1, mat2 = _M2, tmat = _TM} = R -> R
    end.

-spec seed_put(#intstate32{}) -> #intstate32{}.

seed_put(R) ->
    put(tinymt32_seed, R).

-spec seed({integer(), integer(), integer()}) -> 'undefined' | #intstate32{}.

seed({A1, A2, A3}) ->  
    seed(A1, A2, A3).

-spec seed(integer(), integer(), integer()) -> 'undefined' | #intstate32{}.

seed(A1, A2, A3) ->
    seed_put(init_by_list32(seed0(),
			    [A1 band ?TINYMT32_UINT32,
			     A2 band ?TINYMT32_UINT32,
			     A3 band ?TINYMT32_UINT32])).

-spec uniform_s(#intstate32{}) -> {float(), #intstate32{}}.

%% 0.0 <= value < 1.0
uniform_s(R0) ->
    R1 = next_state(R0),
    {temper_float(R1), R1}.

-spec uniform() -> float().

%% 0.0 <= value < 1.0
uniform() ->
    R = case get(tinymt32_seed) of
	    undefined -> seed0();
	    _R -> _R
	end,
    {V, R2} = uniform_s(R),
    put(tinymt32_seed, R2),
    V.

%% 0 <= result < MAX (integer)
-spec uniform_s(pos_integer(), #intstate32{}) -> {pos_integer(), #intstate32{}}.

uniform_s(Max, R) when is_integer(Max), Max >= 1 ->
    Limit = ?TWOPOW32 - (?TWOPOW32 rem Max),
    uniform_s(Max, Limit, R).

uniform_s(M, L, R) ->
    R1 = next_state(R),
    V = temper(R1),
    case V < L of
	true -> {(V rem M) + 1, R1};
	false -> uniform_s(M, L, R1)
    end.

%% 1 <= value <= N
-spec uniform(pos_integer()) -> pos_integer().

uniform(N) when is_integer(N), N >= 1 ->
    R = case get(tinymt32_seed) of
	    undefined -> seed0();
	    _R -> _R
	end,
    {V, R1} = uniform_s(N, R),
    put(tinymt32_seed, R1),
    V.

-spec testloop(pos_integer()) -> list().
		       
testloop(N) ->
    R = seed0(),
    testloop(N, R, []).

testloop(0, _, L) ->
    lists:reverse(L);
testloop(N, R, L) ->
    R1 = next_state(R),
    testloop(N - 1, R1, [temper(R1)|L]).
