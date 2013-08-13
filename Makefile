RELEASE :=	release_manager
NODE :=		release
REL_ROOT :=	rels

N2O :=		deps/n2o/priv/static
N2O_TARGET :=	apps/releaseman/priv/static/nitrogen

default: release n2o-static

n2o-static:
	rm -rf $(N2O)
	rm -rf $(N2O_TARGET)
	ln -sf ../../n2o_scripts $(N2O)
	mkdir -p $(shell dirname $(N2O_TARGET))
	ln -sf ../../../../deps/n2o/priv/static/n2o $(N2O_TARGET)

include Makefile.inc
