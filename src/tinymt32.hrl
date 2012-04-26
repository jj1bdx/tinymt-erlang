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

-type uint32() :: 0..16#ffffffff.

-record(intstate32,
	{status0 :: uint32(),
	 status1 :: uint32(),
	 status2 :: uint32(),
	 status3 :: uint32(),
	 mat1 :: uint32(),
	 mat2 :: uint32(),
	 tmat :: uint32()}).

-define(TINYMT32_SH0, 1).
-define(TINYMT32_SH1, 10).
-define(TINYMT32_SH8, 8).
-define(TINYMT32_MASK, 16#7fffffff).
-define(MASK32, 16#ffffffff).

