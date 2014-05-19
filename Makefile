.PHONY: compile clean ct doc

GNUMAKE=@`sh -c 'if make --version | grep "^GNU Make"; then echo gmake; else echo make; fi' 2>/dev/null`

REBAR=@`sh -c "PATH='$(PATH)':support which rebar\
	||support/getrebar||echo false"`

compile:
	@echo $(GNUMAKE) $(MAKE_VERSION) $(REBAR)
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
