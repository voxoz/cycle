APPS := lager active releaseman
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

test-build:
	curl -v -X POST -d 'github=proger/sync&ref=HEAD' localhost:8989/build

include Makefile.run
