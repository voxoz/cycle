empty :=
space := $(empty) $(empty)

# guessed from current directory
RELEASE ?=	$(lastword $(subst /,$(space),$(CURDIR)))

NODE ?=		node

REL_ROOT ?=	rels
RELEASE_ROOT :=	$(REL_ROOT)/$(RELEASE)
NODE_ROOT :=	$(REL_ROOT)/$(RELEASE)/$(NODE)
NODE_BIN :=	$(NODE_ROOT)/bin/$(NODE)

REBAR ?=	$(or $(shell which rebar 2>/dev/null),./rebar)

SRC_APP_DIR_ROOTS := apps deps
SRC_APP_DIRS :=	$(foreach dir,$(SRC_APP_DIR_ROOTS),$(foreach sub,$(wildcard $(dir)/*),$(shell test -d $(sub) && echo $(sub))))
FOUND_APPS :=	$(foreach appdir,$(SRC_APP_DIRS),$(lastword $(subst /,$(space),$(appdir))))

REL_APPS := $(shell erl -noshell -eval '\
	case file:consult("'$(RELEASE_ROOT)'/reltool.config") of\
		{ok, T} -> io:format("~s~n",\
				[string:join([atom_to_list(S) ||\
					S <- hd([X || {rel, "'$(NODE)'", _, X} <- proplists:get_value(sys, T)])],\
						" ")])\
	end' -eval init:stop'()' 2>/dev/null)

APPS := $(filter $(REL_APPS),$(FOUND_APPS))

default: release

deps: get-deps

get-deps compile clean:
	$(REBAR) $@

start stop attach console:
	$(NODE_BIN) $@

ifneq (,$(findstring init terminating,$(REL_APPS)))

$(warning could not read app list in release "$(RELEASE)" for node "$(NODE)" in $(RELEASE_ROOT)/reltool.config: $(REL_APPS))
generate release clean-release release-links:
	$(error will not generate release)

else

#generate: compile
generate:
	cd $(RELEASE_ROOT); rebar -f $@

clean-release:
	rm -rf $(NODE_ROOT)

# usage: link_app $appname $libpath $subdir
link_app = rm -rf $2/$3; ln -sf $(CURDIR)/$(filter %/$1,$(SRC_APP_DIRS))/$3 $2/$3;

# hack: symlink rebar compile results, etc to generated release for development
release-links:
	@echo relinking released apps..
	$(foreach app,$(APPS),\
		$(call link_app,$(app),$(lastword $(wildcard $(NODE_ROOT)/lib/$(app)-*)),ebin)\
		$(call link_app,$(app),$(lastword $(wildcard $(NODE_ROOT)/lib/$(app)-*)),include)\
		$(call link_app,$(app),$(lastword $(wildcard $(NODE_ROOT)/lib/$(app)-*)),priv))

release: clean-release generate release-links

endif

print:
	@echo "RELEASE: $(RELEASE)"
	@echo "NODE: $(NODE)"
	@echo "REL_ROOT: $(REL_ROOT)"
	@echo "NODE_ROOT: $(NODE_ROOT)"
	@echo "NODE_BIN: $(NODE_BIN)"
	@echo "SRC_APP_DIRS: $(SRC_APP_DIRS)"
	@echo "FOUND_APPS: $(FOUND_APPS)"
	@echo "REL_APPS: $(REL_APPS)"
	@echo "APPS: $(APPS)"


.PHONY: start stop attach clean
