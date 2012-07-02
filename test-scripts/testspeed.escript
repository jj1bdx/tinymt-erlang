#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

%% Note: execute from the root path

main(_) ->

    code:load_file(tinymt32),
    code:load_file(tinymt32_tests),

    tinymt32_tests:test_speed(),
    tinymt32_tests:test_speed(),
    tinymt32_tests:test_speed(),
    tinymt32_tests:test_speed(),
    tinymt32_tests:test_speed(),
    io:format("end of testspeed.escript~n"),
    ok.


