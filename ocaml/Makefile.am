# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for OCaml interface to Redland
#
# Copyright (C) 2000-2005 David Beckett - http://www.dajobe.org/
# Copyright (C) 2000-2005 University of Bristol - http://www.bristol.ac.uk/
# 
# This package is Free Software and part of Redland http://librdf.org/
# 
# It is licensed under the following three licenses as alternatives:
#   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
#   2. GNU General Public License (GPL) V2 or any newer version
#   3. Apache License, V2.0 or any newer version
# 
# You may not use this file except in compliance with at least one of
# the above three licenses.
# 
# See LICENSE.html or LICENSE.txt at the top of this package for the
# full license terms.
# 
# 

RELEASE=@VERSION_RELEASE@

OCAML_STDLIB=@OCAML_STDLIB@
OCAML_INCL=-I$(OCAML_STDLIB)

AM_CPPFLAGS=@CPPFLAGS@ @LIBRDF_CPPFLAGS@ $(OCAML_INCL) $(swig_flags)
AM_CFLAGS=@CFLAGS@ @LIBRDF_CPPFLAGS@ $(MEM) $(swig_flags)

AM_LDFLAGS=@LIBRDF_LDFLAGS@ $(MEM_LIBS)

OCAMLFIND=@OCAMLFIND@

OCAML_PACKAGE=Redland

SWIG_OUTPUTS=$(OCAML_PACKAGE)_wrap.c $(OCAML_PACKAGE).mli $(OCAML_PACKAGE).ml

OCAMLDOC_DIR=ocamldoc
OCAMLDOC_STAMP=ocamldoc-stamp


CAMLRDF_LIB_NAME=camlrdf

ocamldir=$(srcdir)
ocaml_DATA=$(CAMLRDF_LIB_NAME).cmxa $(CAMLRDF_LIB_NAME).a $(CAMLRDF_LIB_NAME).cma META

EXTRA_DIST=README rdf.ml \
	redland-pre.i redland-post.i redland-decl.i redland-typemap.i \
	swig.mli swig.ml \
	$(SWIG_OUTPUTS)

#SUBDIRS=test

CLEANFILES=*.db test-out.rdf \
	$(OCAML_PACKAGE)_wrap.o $(OCAML_PACKAGE)-stamp \
	$(CAMLRDF_LIB_NAME).cmxa $(CAMLRDF_LIB_NAME).a rdf.cmx $(OCAML_PACKAGE).cmx swig.cmx \
	rdf.cmi $(OCAML_PACKAGE).cmi swig.cmi \
	rdf.o $(OCAML_PACKAGE).o swig.o \
	$(OCAML_PACKAGE).bundle

MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT) swig.mli swig.ml

RUN=@REDLAND_RUN@

SWIG_OPTS=-I$(srcdir) -I$(srcdir)/.. -DREDLAND_PRE_I -DREDLAND_POST_I -DREDLAND_DECL_I -DREDLAND_TYPEMAP_I

$(OCAML_PACKAGE)_wrap.c: @REDLAND_SWIG@ redland-post.i redland-decl.i redland-typemap.i
	swig -v -ocaml $(SWIG_OPTS) $(SWIG_OCAML_ARGS) -module $(OCAML_PACKAGE) -o $@ @REDLAND_SWIG@

$(OCAML_PACKAGE)_wrap.o: $(srcdir)/$(OCAML_PACKAGE)_wrap.c
	$(OCAMLFIND) ocamlopt -c -o $@ $(srcdir)/$(OCAML_PACKAGE)_wrap.c \
	-ccopt "$(DEFS) $(SWIG_OPTS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(STANDARD_CFLAGS)"

swig.ml:
	swig -ocaml -co swig.mli
	swig -ocaml -co swig.ml

swig.cmx: swig.ml
	$(OCAMLFIND) ocamlopt swig.mli
	$(OCAMLFIND) ocamlopt -c swig.ml

swig.cmo: swig.ml
	$(OCAMLFIND) ocamlc swig.mli
	$(OCAMLFIND) ocamlc -c swig.ml

$(OCAML_PACKAGE).ml: $(srcdir)/$(OCAML_PACKAGE)_wrap.c

$(OCAML_PACKAGE).cmx: $(srcdir)/$(OCAML_PACKAGE)_wrap.c $(srcdir)/$(OCAML_PACKAGE).ml
	$(OCAMLFIND) ocamlopt $(srcdir)/$(OCAML_PACKAGE).mli
	$(OCAMLFIND) ocamlopt -c $(srcdir)/$(OCAML_PACKAGE).ml

$(OCAML_PACKAGE).cmo: $(srcdir)/$(OCAML_PACKAGE)_wrap.c $(srcdir)/$(OCAML_PACKAGE).ml
	$(OCAMLFIND) ocamlc $(srcdir)/$(OCAML_PACKAGE).mli
	$(OCAMLFIND) ocamlc -c $(srcdir)/$(OCAML_PACKAGE).ml

rdf.cmx: rdf.ml
	$(OCAMLFIND) ocamlopt rdf.mli
	$(OCAMLFIND) ocamlopt -c rdf.ml

rdf.cmo: rdf.ml
	$(OCAMLFIND) ocamlc rdf.mli
	$(OCAMLFIND) ocamlc -c rdf.ml

$(CAMLRDF_LIB_NAME).cmxa: $(OCAML_PACKAGE)-stamp
$(OCAML_PACKAGE)-stamp: swig.cmx $(OCAML_PACKAGE).cmx $(OCAML_PACKAGE)_wrap.o rdf.cmx
	$(OCAMLFIND) ocamlopt -a -linkall -o $(CAMLRDF_LIB_NAME).cmxa -ccopt "$(AM_LDFLAGS)" -ccopt "$(LDFLAGS)" \
	swig.cmx swig.o $(OCAML_PACKAGE).cmx $(OCAML_PACKAGE).o rdf.cmx rdf.o $(OCAML_PACKAGE)_wrap.o
	touch $(OCAML_PACKAGE)-stamp

$(CAMLRDF_LIB_NAME).cma: $(OCAML_PACKAGE)-stamp-byte
$(OCAML_PACKAGE)-stamp-byte: swig.cmo $(OCAML_PACKAGE).cmo $(OCAML_PACKAGE)_wrap.o rdf.cmo
	$(OCAMLFIND) ocamlc -a -linkall -o $(CAMLRDF_LIB_NAME).cma -ccopt "$(AM_LDFLAGS)" -ccopt "$(LDFLAGS)" \
	swig.cmo $(OCAML_PACKAGE).cmo rdf.cmo $(OCAML_PACKAGE)_wrap.o
	touch $(OCAML_PACKAGE)-stamp-byte

LIBRDF_LINKOPTS=-cclib '$(RASQAL_LDFLAGS) $(LIBRDF_LDFLAGS)'

META:
	sed -e "s%LINKOPTS%$(LIBRDF_LINKOPTS)%g" META.in > META

install-ocamlDATA: $(ocaml_DATA)
	$(OCAMLFIND) install $(CAMLRDF_LIB_NAME) $(srcdir)/META \
	$(srcdir)/$(CAMLRDF_LIB_NAME).cmxa $(CAMLRDF_LIB_NAME).a $(srcdir)/$(CAMLRDF_LIB_NAME).cma

uninstall-ocamlDATA: $(ocaml_DATA)
	$(OCAMLFIND) remove $(CAMLRDF_LIB_NAME)

check: $(CAMLRDF_LIB_NAME).cmxa
	$(OCAMLFIND) ocamlopt -o test.opt $(CAMLRDF_LIB_NAME).cmxa test.ml $(LIBRDF_LINKOPTS)
	$(OCAMLFIND) ocamlopt -o example.opt $(CAMLRDF_LIB_NAME).cmxa example.ml $(LIBRDF_LINKOPTS)
	$(srcdir)/test.opt #&& $(srcdir)/example.opt

docs: $(OCAMLDOC_STAMP)
$(OCAMLDOC_STAMP):
	mkdir -p $(OCAMLDOC_DIR); ocamldoc -html rdf.mli -d $(OCAMLDOC_DIR) && touch $@

clean-local:
	rm -rf $(OCAMLDOC_DIR) $(OCAMLDOC_STAMP)
