.PHONY: compile clean doc eunit

REBAR=@`sh -c "PATH='$(PATH)':support which rebar\
	||support/getrebar||echo false"`

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

doc:
	$(REBAR) doc

eunit:
	$(REBAR) eunit

speed:
	escript test-scripts/testspeed.escript

dialyzer:
	        dialyzer src/*.erl

firsttime-dialyzer:
	        dialyzer --build_plt --apps kernel stdlib erts mnesia eunit crypto
