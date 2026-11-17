# regionseq: a sequence access method for ids that several regions mint at once.
#
# Out of tree, against a PostgreSQL that has the sequence access method patches
# ("Sequence Access Methods, round two"):
#
#   make && make install
#   make installcheck       # on a server started with shared_preload_libraries = 'regionseq'
#                           # and regionseq.region_id = 5
#
# In the contrib/ directory of such a source tree: make NO_PGXS=1

MODULES = regionseq

EXTENSION = regionseq
DATA = regionseq--1.0.sql
DOCS = README.md

REGRESS = regionseq

ifdef NO_PGXS
subdir = contrib/regionseq
top_builddir = ../..
include $(top_builddir)/src/Makefile.global
include $(top_srcdir)/contrib/contrib-global.mk
REGRESS_OPTS = --temp-config $(top_srcdir)/contrib/regionseq/regionseq.conf
NO_INSTALLCHECK = 1
else
PG_CONFIG ?= pg_config
PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
endif
