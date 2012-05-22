#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

%% Note: execute from the root path

main(_) ->

    code:load_file(fprof),
    code:load_file(tinymt32),
    code:load_file(tinymt32_tests),

    fprof:trace(start),
    tinymt32_tests:test_speed(),
    fprof:trace(stop),
    fprof:profile(),
    fprof:analyse({dest, "test_speed.txt"}),

    io:format("end of fprof.escript~n"),
    ok.


