.PHONY: compile clean ct doc

REBAR=@`sh -c "PATH='$(PATH)':support which rebar\
	||support/getrebar||echo false"`

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

ct:
	$(REBAR) ct

doc:
	$(REBAR) doc

speed:
	escript test-scripts/testspeed.escript

dialyzer:
	        dialyzer src/*.erl

firsttime-dialyzer:
	        dialyzer --build_plt --apps kernel stdlib erts mnesia eunit crypto
