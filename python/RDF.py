# 
# RDF.py - Redland Python RDF module
#
# $Id$
#
# Copyright (C) 2000 David Beckett - http://purl.org/net/dajobe/
# Institute for Learning and Research Technology, University of Bristol.
#
#    This package is Free Software available under either of two licenses
#    (see FAQS.html to see why):
# 
# 1. The GNU Lesser General Public License (LGPL)
# 
#    See http://www.gnu.org/copyleft/lesser.html or COPYING.LIB for the
#    full license text.
#      _________________________________________________________________
# 
#      Copyright (C) 2000 David Beckett, Institute for Learning and
#      Research Technology, University of Bristol. All Rights Reserved.
# 
#      This library is free software; you can redistribute it and/or
#      modify it under the terms of the GNU Lesser General Public License
#      as published by the Free Software Foundation; either version 2 of
#      the License, or (at your option) any later version.
# 
#      This library is distributed in the hope that it will be useful, but
#      WITHOUT ANY WARRANTY; without even the implied warranty of
#      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#      Lesser General Public License for more details.
# 
#      You should have received a copy of the GNU Lesser General Public
#      License along with this library; if not, write to the Free Software
#      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
#      USA
#      _________________________________________________________________
# 
#    NOTE - under Term 3 of the LGPL, you may choose to license the entire
#    library under the GPL. See COPYING for the full license text.
# 
# 2. The Mozilla Public License
# 
#    See http://www.mozilla.org/MPL/MPL-1.1.html or MPL.html for the full
#    license text.
# 
#    Under MPL section 13. I declare that all of the Covered Code is
#    Multiple Licensed:
#      _________________________________________________________________
# 
#      The contents of this file are subject to the Mozilla Public License
#      version 1.1 (the "License"); you may not use this file except in
#      compliance with the License. You may obtain a copy of the License
#      at http://www.mozilla.org/MPL/
# 
#      Software distributed under the License is distributed on an "AS IS"
#      basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
#      the License for the specific language governing rights and
#      limitations under the License.
# 
#      The Initial Developer of the Original Code is David Beckett.
#      Portions created by David Beckett are Copyright (C) 2000 David
#      Beckett, Institute for Learning and Research Technology, University
#      of Bristol. All Rights Reserved.
# 
#      Alternatively, the contents of this file may be used under the
#      terms of the GNU Lesser General Public License, in which case the
#      provisions of the LGPL License are applicable instead of those
#      above. If you wish to allow use of your version of this file only
#      under the terms of the LGPL License and not to allow others to use
#      your version of this file under the MPL, indicate your decision by
#      deleting the provisions above and replace them with the notice and
#      other provisions required by the LGPL License. If you do not delete
#      the provisions above, a recipient may use your version of this file
#      under either the MPL or the LGPL License.
#

__version__ = "0.1"

__debug__ = 0

import sys
import string

import Redland;


class world:
  """Core RDF class"""

  def __init__(self,digest_name=None,uri_hash=None):
    """Create new RDF object (constructor)"""
    # Keep a circular reference to this object so it is deleted last
    self.me=self

    Redland.librdf_init_world(digest_name, uri_hash)

  # __init__ ()

  def close(self):
    """Destroy RDF object (destructor)."""
    if __debug__:
      print "Destroying RDF.world"
    self.me=None
    Redland.librdf_destroy_world()

  def debug(self,value=0):
    global __debug__
    if value:
      __debug__=value
    else:
      return __debug__


# end class world


class node:

  # CONSTRUCTOR
  def __init__(self, args={}):
    """Create an RDF Node (constructor)."""
    if __debug__:
      print "Creating RDF.node args=",args
    self.node=None
    self.free_me=1

    if args.has_key('uri_string'):
      self.node=Redland.librdf_new_node_from_uri_string(args['uri_string'])
    elif args.has_key('uri'):
      self.node=Redland.librdf_new_node_from_uri(args['uri'].uri)
    elif args.has_key('literal'):
      if args.has_key('xml_language'):
        xml_language=args['xml_language']
      else:
        xml_language=""
      if args.has_key('xml_space'):
        xml_space=args['xml_space']
      else:
        xml_space=0
      if args.has_key('is_wf_xml'):
        is_wf_xml=args['is_wf_xml']
      else:
        is_wf_xml=0
      self.node=Redland.librdf_new_node_from_literal(args['literal'],xml_language,xml_space,is_wf_xml)
    elif args.has_key('node'):
      self.node=Redland.librdf_new_node_from_node(args['node'].node)
    elif args.has_key('from_object'):
      # internal constructor to build an object from a node created
      # by librdf e.g. from the result of a iterator->next operation
      # this is always shared (at present) so should not be freed
      self.node=args['from_object']
      self.free_me=args['free_statements']
    else:
      self.node=Redland.librdf_new_node()

  # DESTRUCTOR
  def __del__(self):
    """Free an RDF Node (destructor)."""
    if __debug__:
      print "Destroying RDF.node"
    if self.node and self.free_me:
      if __debug__:
        print "Deleting Redland node object"
      Redland.librdf_free_node(self.node)

  def uri (self, uri=None):
    """Set/get the node URI."""
    if not uri:
      return Redland.librdf_node_get_uri(self.node)

    return Redland.librdf_node_set_uri(self.node, uri.uri)

  def type (self, type=None):
    """Set/get the node type."""
    if not type:
      return Redland.librdf_node_get_type(self.node)

    return Redland.librdf_node_set_type(self.node, type)

  def literal_value(self):
    """Get a literal node string value."""
    return Redland.librdf_node_get_literal_value(self.node)

  def literal_value_language(self):
    """Get a literal node language value."""
    return Redland.librdf_node_get_literal_value_language(self.node)

  def literal_value_xml_space(self):
    """Get a literal node XML space value."""
    return Redland.librdf_node_get_literal_value_xml_space(self.node)

  def literal_value_is_wf_xml(self):
    """Get a literal node is WF XML value."""
    return Redland.librdf_node_get_literal_value_is_wf_xml(self.node)

  def set_literal_value (self,value,xml_language,xml_space,is_wf_xml):
    """Set a literal node string value."""
    return Redland.librdf_node_set_literal_value(self.node,value,xml_language,xml_space,is_wf_xml)

  def __str__(self):
    """Get a string representation of an RDF Node."""
    return Redland.librdf_node_to_string(self.node)

# end class node


class statement:

  # CONSTRUCTOR
  def __init__(self, args={}):
    """Create an RDF Statement (constructor)."""
    if __debug__:
      print "Creating RDF.statement object args",args
    self.statement=None
    self.free_me=1

    if args.has_key('statement'):
      self.statement=Redland.librdf_new_statement_from_statement(args['statement'].statement)
    elif args.has_key('subject'):
      subject=args['subject']
      predicate=args['predicate']
      object=args['object']

      # Replace python-land 'None' with C-land 'NULL' pointers to Redland.librdf_node
      if not subject:
	s=None
      else:
	s=subject.node

      if not predicate:
	p=None
      else:
	p=predicate.node

      if not object:
	o=None
      else:
	o=object.node

      self.statement=Redland.librdf_new_statement_from_nodes(s, p, o)

      # Zap the incoming librdf node objects since they are now owned by the
      # librdf statement object self.statement
      if subject:
	subject.node=None
      if predicate:
	predicate.node=None
      if object:
	object.node=None

    elif args.has_key('from_object') and args.has_key('free_statements'): 
      # internal constructor to build an object from a statement created
      # by librdf e.g. from the result of a stream.next operation
      self.statement=args['from_object']
      self.free_me=args['free_statements']

    else:
      self.statement=Redland.librdf_new_statement()

  # DESTRUCTOR
  def __del__(self):
    if __debug__:
      print "Destroying RDF.statement"
    if self.statement and self.free_me:
      if __debug__:
        print "Deleting Redland statement object"
      Redland.librdf_free_statement(self.statement)

  def subject (self,subject=None):
    if subject:
      return Redland.librdf_statement_get_subject(self.statement)

    # Zap the incoming librdf node object since it is now owned by the
    # librdf statement object self.statement
    subject.node=None
    return Redland.librdf_statement_set_subject(self.statement,subject)

  def predicate (self,predicate=None):
    if predicate:
      return Redland.librdf_statement_get_predicate(self.statement)

    # Zap the incoming librdf node object since it is now owned by the
    # librdf statement object self.statement
    predicate.node=None
    return Redland.librdf_statement_set_predicate(self.statement,predicate)

  def object (self):
    if object:
      return Redland.librdf_statement_get_object(self.statement)

    # Zap the incoming librdf node object since it is now owned by the
    # librdf statement object self.statement
    object.node=None
    return Redland.librdf_statement_set_object(self.statement,object)

  def __str__ (self):
    return Redland.librdf_statement_to_string(self.statement)

# end class statement


class model:

  # CONSTRUCTOR
  def __init__(self, storage, args={}):
    """Create an RDF Model (constructor)."""
    if __debug__:
      print "Creating RDF.model args=",args
    self.model=None
    self.storage=None

    if args.has_key('options_string'):
      self.model=Redland.librdf_new_model(storage.storage, args['options_string'])
    elif args.has_key('options_hash'):
      self.model=Redland.librdf_new_model_with_options(storage.storage, args['options_hash'].hash)
    elif args.has_key('model'):
      self.model=Redland.librdf_new_model_from_model(storage.storage,
      args['model'].model)
    else:
      self.model=Redland.librdf_new_model(storage.storage, None)

    if self.model == "NULL":
      self.model=None
      raise "new RDF.model failed"
    else:
      # keep a reference around so storage object is destroyed after this
      self.storage=storage

  # DESTRUCTOR
  def __del__(self):
    if __debug__:
      print "Destroying RDF.model "
    if self.model:
      Redland.librdf_free_model(self.model)

  def size(self):
    return Redland.librdf_model_size(self.model)

  def add(self,subject,predicate,object):
    return Redland.librdf_model_add(self.model, subject.node, predicate.node, object.node);

  def add_string_literal_statement (self,subject,predicate,string,xml_language,xml_space,is_wf_xml):
    return Redland.librdf_model_add_string_literal_statement(self.model, subject.node, predicate.node, string, xml_language, xml_space, is_wf_xml)

  def add_statement (self,statement):
    Redland.librdf_model_add_statement(self.model,statement.statement)
    # Remove librdf_statement reference from RDF.statement object
    statement.statement=None

  def add_statements (self,statement_stream):
    return Redland.librdf_model_add_statements(self.model, statement_stream.stream)

  def remove_statement (self,statement):
    return Redland.librdf_model_remove_statement(self.model, statement.statement)

  def contains_statement (self,statement):
    return Redland.librdf_model_contains_statement(self.model, statement.statement)

  def serialise (self):
    my_stream=Redland.librdf_model_serialise(self.model)
    return stream(my_stream,self,1)

  def find_statements (self,statement):
    my_stream=Redland.librdf_model_find_statements(self.model, statement.statement)
    return stream(my_stream,self,0)

  def get_sources (self,arc,target):
    my_iterator=Redland.librdf_model_get_sources(self.model, arc.node, target.node)
    return iterator(my_iterator,self)

  def get_arcs (self,source,target):
    my_iterator=Redland.librdf_model_get_arcs(self.model, source.node, target.node)
    return iterator(my_iterator,self)

  def get_targets (self,source,arc):
    my_iterator=Redland.librdf_model_get_targets(self.model, source.node, arc.node)
    return iterator(my_iterator,self)

#end class model


class iterator:

  # CONSTRUCTOR
  def __init__(self,object,creator):
    """Create an RDF Iterator (constructor)."""
    if __debug__:
      print "Creating RDF.iterator object=",object,"creator=",creator
    self.iterator=object;
    # Keep around a reference to the object that created the iterator
    # so that python does not destroy us before them.
    self.creator=creator;

  # DESTRUCTOR
  def __del__(self):
    if __debug__:
      print "Destroying RDF.iterator"
    Redland.librdf_free_iterator(self.iterator)

  def have_elements (self):
    return Redland.librdf_iterator_have_elements(self.iterator)

  def next (self):
    my_node=Redland.librdf_iterator_get_next(self.iterator)
    if my_node == "NULL":
      return None
    # return a new (1) node (2)owned by the librdf iterator object
    # Reasons: (1) at the user API level the iterator only returns nodes
    #          (2) the node returned is shared with the iterator
    return node({"from_object" : my_node})

#end class iterator


class stream:

  # CONSTRUCTOR
  def __init__(self, object, creator, free_statements):
    """Create an RDF Stream (constructor)."""
    if __debug__:
      print "Creating RDF.stream for object",object,"creator",creator,"free_statements",free_statements

    self.stream=object;
    # Keep around a reference to the object that created the stream
    # so that perl does not destroy us before them.
    self.creator=creator;
    # should the resulting statements be freed?
    self.free_statements=free_statements;

  # DESTRUCTOR
  def __del__(self):
    if __debug__:
      print "Destroying RDF.stream"
    Redland.librdf_free_stream(self.stream)

  def end (self):
    if not self.stream:
      return 1
    return Redland.librdf_stream_end(self.stream)

  def next (self):
    if not self.stream:
      return None
    # return a new statement created by the librdf stream object
    my_statement=Redland.librdf_stream_next(self.stream)
    if my_statement == "NULL":
      return None
    return statement({"from_object" : my_statement, "free_statements" : self.free_statements})

# end class stream


class storage:

  # CONSTRUCTOR
  def __init__(self, args):
    """Create an RDF Storage (constructor)."""
    if __debug__:
      print "Creating RDF.storage args=",args
    self.storage=None

    if args.has_key('storage_name') and args.has_key('name') and args.has_key('options_string'):
      self.storage=Redland.librdf_new_storage(args['storage_name'] ,args['name'], args['options_string']);
    elif args.has_key('storage'):
      self.storage=Redland.librdf_new_storage_from_storage(args['storage'].storage);
    else:
      raise "new RDF.storage failed - illegal arguments"

    if self.storage == "NULL":
      self.storage=None
      raise "new RDF.storage failed"


  # DESTRUCTOR
  def __del__(self):
    if __debug__:
      print "Destroying RDF.storage"
    if self.storage:
      Redland.librdf_free_storage(self.storage)

# end class storage


class uri:

  # CONSTRUCTOR
  def __init__(self, args):
    """Create an RDF Uri (constructor)."""
    if __debug__:
      print "Creating RDF.uri args=",args
    self.uri=None

    if args.has_key('string'):
     self.uri=Redland.librdf_new_uri(string)
    elif args.has_key('uri'):
      # FIXME: If the URI is a python URI ... need to use the above constructor
      # if isa(uri, <python uri object>):
      #   self.uri=Redland.librdf_new_uri(uri.as_string)
      self.uri=Redland.librdf_new_uri_from_uri(uri.uri)

  # DESTRUCTOR
  def __del__(self):
    if __debug__:
      print "Destroying RDF.uri"
    if self.uri:
      Redland.librdf_free_uri(self.model)

  def __str__(self):
    """Get a string representation of an RDF URI."""
    return Redland.librdf_uri_to_string(self.uri)

# end class uri


class parser:

  # CONSTRUCTOR
  def __init__(self, name, mime_type="", uri=None):
    """Create an RDF Parser (constructor)."""
    if __debug__:
      print "Creating RDF.parser name=",name,"mime_type=",mime_type,"uri=",uri

    if uri:
      uri=uri.uri
  
    self.parser=Redland.librdf_new_parser(name,mime_type,uri)

  # DESTRUCTOR
  def __del__(self):
    if __debug__:
      print "Destroying RDF.parser"
    if self.parser:
      Redland.librdf_free_parser(self.parser)

  def parse_as_stream (self,uri,base_uri):
    my_stream=Redland.librdf_parser_parse_as_stream(self.parser,uri.uri, base_uri.uri)
    return stream(my_stream,self,1)

  def parse_into_model (self,uri,base_uri,model):
    return Redland.librdf_parser_parse_into_model(self.parser,uri.uri,base_uri.uri,model.model)

  def feature (self,uri,value=None):
    # FIXME: if uri not <an RDF.uri object>:
    #  uri=uri({'string'} : uri_string})  

    if not value:
      return Redland.librdf_parser_get_feature(self.parser,uri.uri)

    return Redland.librdf_parser_set_feature(self.parser,uri.uri,value)

# end class parser