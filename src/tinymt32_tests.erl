%% (This is a simplified BSD license.)
%%
%% Copyright (c) 2012-2014 Kenji Rikitake and Kyoto University.
%% All rights reserved.
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

%% From random module
-type ran() :: {integer(), integer(), integer()}.

-spec test_speed_tinymt_uniform_rec1([float()], non_neg_integer(), non_neg_integer(),
    pos_integer(), tinymt32:intstate32()) -> 'ok'.

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

-spec test_speed_orig_uniform_n_rec1([integer()], non_neg_integer(), non_neg_integer(), pos_integer(), ran()) -> 'ok'.

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

-spec test_speed_tinymt_uniform_n_rec1([non_neg_integer()], non_neg_integer(), non_neg_integer(), pos_integer(), tinymt32:intstate32()) -> 'ok'.

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

-spec test_speed_orig_uniform_rec1([float()], non_neg_integer(), non_neg_integer(), pos_integer(), ran()) -> 'ok'.

test_speed_orig_uniform_rec1(Acc, 0, _, _, _) ->
    _ = lists:reverse(Acc),
    ok;
test_speed_orig_uniform_rec1(Acc, X, 0, R, I) ->
    _ = lists:reverse(Acc),
    test_speed_orig_uniform_rec1([], X - 1, R, R, I);
test_speed_orig_uniform_rec1(Acc, X, Q, R, I) ->
    {F, I2} = random:uniform_s(I),
    test_speed_orig_uniform_rec1([F|Acc], X, Q - 1, R, I2).

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
