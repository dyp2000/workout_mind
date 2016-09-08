#!/usr/bin/colormake
PATH=$PATH:/bin:/usr/bin:/usr/local/lib/erlang/bin:/usr/local/lib/erlang/lib:/usr/local/bin
PRJ_ROOT=$(shell pwd)
PRJ_NAME=test06
ERLC_FLAGS=-smp -W 
ERLC_DEBUG_FLAGS=+debug_info -smp -W 
REBAR=/usr/local/bin/rebar

all: compile

compile:
	@echo "== Server compile =="
	@cd $(PRJ_ROOT)
	@rm -f $(PRJ_ROOT)/ebin/*.*
	@cp $(PRJ_ROOT)/src/$(PRJ_NAME).app.src $(PRJ_ROOT)/ebin/$(PRJ_NAME).app
	erlc +native -v -smp -o $(PRJ_ROOT)/ebin $(ERLC_FLAGS) $(PRJ_ROOT)/src/*.erl

debug:
	@echo "== Server DEBUG compile =="
	@cd $(PRJ_ROOT)
	@rm -f $(PRJ_ROOT)/ebin/*.*
	@cp $(PRJ_ROOT)/src/$(PRJ_NAME).app.src $(PRJ_ROOT)/ebin/$(PRJ_NAME).app
	erlc +native -v -smp -o $(PRJ_ROOT)/ebin $(ERLC_DEBUG_FLAGS) $(PRJ_ROOT)/src/*.erl

get-deps:
	$(REBAR) get-deps compile

deps-recompile:
	$(REBAR) compile

update:
	$(REBAR) update-deps compile

clean:
	rm -f $(PRJ_ROOT)/erl_crash.dump
	$(REBAR) clean
