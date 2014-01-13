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

-module(tinymt32_tests).

-export([test_speed/0]).

-include("tinymt32.hrl").

%% From random module
-type ran() :: {integer(), integer(), integer()}.

-spec test_speed_tinymt_uniform_rec1([uint32()], non_neg_integer(), non_neg_integer(), pos_integer(), #intstate32{}) -> 'ok'.

test_speed_tinymt_uniform_rec1(Acc, 0, _, _, _) ->
    _ = lists:reverse(Acc),
    ok;
test_speed_tinymt_uniform_rec1(Acc, X, 0, R, I) ->
    _ = lists:reverse(Acc),
    test_speed_tinymt_uniform_rec1([], X - 1, R, R, I);
test_speed_tinymt_uniform_rec1(Acc, X, Q, R, I) ->
    {F, I2} = tinymt32:uniform_s(I),
    test_speed_tinymt_uniform_rec1([F|Acc], X, Q - 1, R, I2).

-spec test_speed_tinymt_uniform(non_neg_integer(), non_neg_integer()) -> non_neg_integer().

test_speed_tinymt_uniform(P, Q) ->
    _ = statistics(runtime),
    I = tinymt32:seed(),
    ok = test_speed_tinymt_uniform_rec1([], P, Q, Q, I),
    {_, T} = statistics(runtime),
    T.

-spec test_speed_orig_uniform_n_rec1([uint32()], non_neg_integer(), non_neg_integer(), pos_integer(), ran()) -> 'ok'.

test_speed_orig_uniform_n_rec1(Acc, 0, _, _, _) ->
    _ = lists:reverse(Acc),
    ok;
test_speed_orig_uniform_n_rec1(Acc, X, 0, R, I) ->
    _ = lists:reverse(Acc),
    test_speed_orig_uniform_n_rec1([], X - 1, R, R, I);
test_speed_orig_uniform_n_rec1(Acc, X, Q, R, I) ->
    {F, I2} = random:uniform_s(10000, I),
    test_speed_orig_uniform_n_rec1([F|Acc], X, Q - 1, R, I2).

-spec test_speed_orig_uniform_n(non_neg_integer(), non_neg_integer()) -> non_neg_integer().

test_speed_orig_uniform_n(P, Q) ->
    _ = statistics(runtime),
    I = random:seed(),
    ok = test_speed_orig_uniform_n_rec1([], P, Q, Q, I),
    {_, T} = statistics(runtime),
    T.

-spec test_speed_tinymt_uniform_n_rec1([uint32()], non_neg_integer(), non_neg_integer(), pos_integer(), #intstate32{}) -> 'ok'.

test_speed_tinymt_uniform_n_rec1(Acc, 0, _, _, _) ->
    _ = lists:reverse(Acc),
    ok;
test_speed_tinymt_uniform_n_rec1(Acc, X, 0, R, I) ->
    _ = lists:reverse(Acc),
    test_speed_tinymt_uniform_n_rec1([], X - 1, R, R, I);
test_speed_tinymt_uniform_n_rec1(Acc, X, Q, R, I) ->
    {F, I2} = tinymt32:uniform_s(10000, I),
    test_speed_tinymt_uniform_n_rec1([F|Acc], X, Q - 1, R, I2).

-spec test_speed_tinymt_uniform_n(non_neg_integer(), non_neg_integer()) -> non_neg_integer().

test_speed_tinymt_uniform_n(P, Q) ->
    _ = statistics(runtime),
    I = tinymt32:seed(),
    ok = test_speed_tinymt_uniform_n_rec1([], P, Q, Q, I),
    {_, T} = statistics(runtime),
    T.

-spec test_speed_orig_uniform_rec1([uint32()], non_neg_integer(), non_neg_integer(), pos_integer(), ran()) -> 'ok'.

test_speed_orig_uniform_rec1(Acc, 0, _, _, _) ->
    _ = lists:reverse(Acc),
    ok;
test_speed_orig_uniform_rec1(Acc, X, 0, R, I) ->
    _ = lists:reverse(Acc),
    test_speed_orig_uniform_n_rec1([], X - 1, R, R, I);
test_speed_orig_uniform_rec1(Acc, X, Q, R, I) ->
    {F, I2} = random:uniform_s(I),
    test_speed_orig_uniform_n_rec1([F|Acc], X, Q - 1, R, I2).

-spec test_speed_orig_uniform(non_neg_integer(), non_neg_integer()) -> non_neg_integer().

test_speed_orig_uniform(P, Q) ->
    _ = statistics(runtime),
    I = random:seed(),
    ok = test_speed_orig_uniform_rec1([], P, Q, Q, I),
    {_, T} = statistics(runtime),
    T.

-spec test_speed() -> 'ok'.

test_speed() ->
    io:format("{orig_uniform, orig_uniform_n, tinymt_uniform, tinymt_uniform_n}~n~p~n",
              [{test_speed_orig_uniform(100, 10000),
                test_speed_orig_uniform_n(100, 10000),
                test_speed_tinymt_uniform(100, 10000),
                test_speed_tinymt_uniform_n(100, 10000)}
              ]).

%% EUnit test functions

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

testloop_test() ->
    Refval = test_refval(),
    Testval = tinymt32:testloop(length(Refval)),
    ?assertEqual(Refval, Testval).

%% @doc simple testing function as used in EUnit

simple_test_() ->
    [
     ?_assertMatch(ok, testloop_test())
    ].

test_refval() ->
    [
     981918433, 
     3715302833, 
     2387538352, 
     3591001365, 
     3820442102, 
     2114400566, 
     2196103051, 
     2783359912, 
     764534509, 
     643179475, 
     1822416315, 
     881558334, 
     4207026366, 
     3690273640, 
     3240535687, 
     2921447122, 
     3984931427, 
     4092394160, 
     44209675, 
     2188315343, 
     2908663843, 
     1834519336, 
     3774670961, 
     3019990707, 
     4065554902, 
     1239765502, 
     4035716197, 
     3412127188, 
     552822483, 
     161364450, 
     353727785, 
     140085994, 
     149132008, 
     2547770827, 
     4064042525, 
     4078297538, 
     2057335507, 
     622384752, 
     2041665899, 
     2193913817, 
     1080849512, 
     33160901, 
     662956935, 
     642999063, 
     3384709977, 
     1723175122, 
     3866752252, 
     521822317, 
     2292524454, 
     2554388431, 
     3919761922, 
     2984019591, 
     1885567152, 
     1673658720, 
     2651182511, 
     3306657681, 
     909221937, 
     684822786, 
     1522786736, 
     2405507378, 
     1937983881, 
     1482169516, 
     4178854305, 
     208993333, 
     3331261356, 
     3763091809, 
     3960056579, 
     1103076587, 
     1517817904, 
     2027272493, 
     4288375315, 
     2918101968, 
     2988616367, 
     1921035354, 
     4105268863, 
     1317828195, 
     3918550276, 
     3397886683, 
     4143227599, 
     4257789512, 
     1004753582, 
     1168941243, 
     4028344166, 
     1489679262, 
     681878218, 
     2898758405, 
     529944940, 
     3582614078, 
     953995809, 
     1575179133, 
     1162490265, 
     1962685354, 
     1700768287, 
     2373844329, 
     2936458386, 
     2381852499, 
     189647353, 
     1472838906, 
     3698288610
    ].

-endif. % TEST
