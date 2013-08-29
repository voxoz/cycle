APPS := lager releaseman
ERL_ARGS := -args_file run/vm.args -config run/sys.config

N2O :=		deps/n2o/priv/static
N2O_TARGET :=	apps/releaseman/priv/static/nitrogen

default: compile n2o-static

n2o-static:
	rm -rf $(N2O)
	rm -rf $(N2O_TARGET)
	ln -sf ../../n2o_scripts $(N2O)
	mkdir -p $(shell dirname $(N2O_TARGET))
	ln -sf ../../../../deps/n2o/priv/static/n2o $(N2O_TARGET)

sync:
	fsevent_watch -F . \
		| tee /dev/stderr \
		| env PERLIO=:raw perl -ne 's#.*\t.*\t$$ENV{"PWD"}/(apps|deps)/(\w+)/(?!ebin)#\2# && print "$$1=$$2\n"' \
		| xargs -n1 rebar compile

test-build:
	curl -v -X POST -d 'github=proger/sync&ref=HEAD' localhost:8989/build

include Makefile.run
