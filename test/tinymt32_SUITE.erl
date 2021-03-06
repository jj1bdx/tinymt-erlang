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

-module(tinymt32_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([testloop_test/1,
         testloop2_test/1]).
 
all() ->
    [testloop_test, testloop2_test].

-spec testloop(pos_integer()) -> list().

testloop(N) ->
    R = tinymt32:seed0(),
    testloop(N, R, []).

testloop(0, _, L) ->
    lists:reverse(L);
testloop(N, R, L) ->
    R1 = tinymt32:next_state(R),
    testloop(N - 1, R1, [tinymt32:temper(R1)|L]).

testloop_test(_Config) ->
    Refval = test_refval(),
    Testval = testloop(length(Refval)),
    Refval =:= Testval.

-spec testloop2(pos_integer()) -> list().

testloop2(N) ->
    _ = tinymt32:setgenparams(
         {16#877810ef, 16#fc38ff0f, 16#c7fb7fff}),
    testloop2(N, get(tinymt32_seed), []).

testloop2(0, _, L) ->
    lists:reverse(L);
testloop2(N, R, L) ->
    R1 = tinymt32:next_state(R),
    testloop2(N - 1, R1, [tinymt32:temper(R1)|L]).

testloop2_test(_Config) ->
    Refval = test2_refval(),
    Testval = testloop2(length(Refval)),
    Refval =:= Testval.

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

test2_refval() ->
    [3404636385,4205598316,329097095,1977676128,2321853530,184394170,1178838988,
     414454771,2564371661,3532916881,2588320494,3984127785,3315887835,2717739930,
     939551688,3007879743,2897288949,3444423296,2635246610,1726558932,47896918,
     3179788588,2674534142,2895154665,1978931467,929893650,1922383368,3686941556,
     3237914934,1733932384,1046098280,2104864095,484184724,3860964083,295784133,
     2320241333,3417426506,2590981271,1559069134,1043594792,615341058,3159080264,
     703318990,1128245658,1947983477,1819592205,536534738,2433537154,2134794957,
     3330812735,3197284920,1406792856,630747647,112532988,3593261487,1811709349,
     2083244224,2296214925,4176440308,2366008754,3829408091,2199475737,3364112033,
     2511787724,706985181,3137972836,2916138782,63053652,3447125977,1262416059,
     4071067697,992295325,3505166443,2243530867,1433707076,377918929,2992634430,
     1545606441,142378660,775970106,3859716799,3350314113,1029527444,1643815264,
     2179145501,1756869897,193959959,2998448946,1073863843,2219913981,2594533645,
     2396779772,3601944501,2988952794,1231622313,2895888770,1087393236,1814463467,
     1466261541,3604331833].

