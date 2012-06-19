(* 
 * Redland API for OCaml 
 * Copyright (C) 2012 Codinuum Software Lab - http://codinuum.com/
 *
 *
 * This package is Free Software and based on Redland http://librdf.org/
 * 
 * It is licensed under the following three licenses as alternatives:
 *   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
 *   2. GNU General Public License (GPL) V2 or any newer version
 *   3. Apache License, V2.0 or any newer version
 *
 * This one file rdf.ml is also available under the following license:
 *
 *   4. BSD License without advertising (aka MIT license)
 *      from http://www.opensource.org/licenses/mit-license.html
 *
 *      -------------------------------------------------------------------
 *      Permission is hereby granted, free of charge, to any person
 *      obtaining a copy of this software and associated documentation
 *      files (the "Software"), to deal in the Software without
 *      restriction, including without limitation the rights to use, copy,
 *      modify, merge, publish, distribute, sublicense, and/or sell copies
 *      of the Software, and to permit persons to whom the Software is
 *      furnished to do so, subject to the following conditions:
 *      
 *      The above copyright notice and this permission notice shall be
 *      included in all copies or substantial portions of the Software.
 *      
 *      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 *      EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 *      MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 *      NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 *      BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 *      ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 *      CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 *      SOFTWARE.
 *     --------------------------------------------------------------------
 *
 * You may not use this file except in compliance with at least one of
 * the above four licenses.
 * 
 * See LICENSE.html or LICENSE.txt at the top of this package for the
 * full license terms.
 *
 *)

(** A OCaml interface to the Redland RDF library.
   
   See http://librdf.org/ for details of the Redland library.
 *)

type repr
(** Internal representation of Redland objects. *)

exception NodeTypeError of string


type node_type = T_unknown | T_resource | T_literal | T_blank
(** Type of RDF node. A node may be a resource (a URI), a literal, or a blank node. *)


(** URI (Uniform Resource Identifier). *)
module Uri : sig
(** Redland Uri class. *)
  class c : string -> object ('self)
    method repr : repr

    method is_valid  : bool
    method to_string : string
    method equals    : 'self -> bool
    method clone     : 'self
  end
  val null : c
end


type literal_value = {
    lv_string   : string;
    lv_language : string;
    lv_datatype : Uri.c;
  }
(** RDF literal. *)


(** RDF node. *)
module Node : sig
(** Redland Node class. *)
  class c : ?uri_string:string -> ?uri:Uri.c -> ?literal:string -> ?datatype:Uri.c ->
    ?is_wf_xml:int -> ?xml_language:string -> ?blank:string -> unit -> object ('self)
      method repr : repr

      method is_valid             : bool
      method get_type             : node_type
      method is_resource          : bool
      method is_literal           : bool
      method is_blank             : bool
      method equals               : 'self -> bool
      method get_uri              : Uri.c
      method get_literal_value    : literal_value
      method get_blank_identifier : string
      method to_string            : string
      method clone                : 'self
    end
  val type_to_string     : node_type -> string
  val from_uri_string    : string -> c
  val from_uri           : Uri.c -> c
  val from_literal       : ?xml_language:string -> ?is_wf_xml:int -> string -> c
  val from_typed_literal : ?xml_language:string -> ?datatype:Uri.c -> string -> c
  val from_blank         : string -> c
  val null               : c
end

(** RDF statements (triples). *)
module Statement : sig
(** Redland Statement class. *)
  class c : ?subj_node:Node.c -> ?subj_uri:Uri.c -> ?subj_uri_string:string -> ?subj_blank:string ->
    ?pred_node:Node.c -> ?pred_uri:Uri.c -> ?pred_uri_string:string -> 
      ?obj_node:Node.c -> ?obj_uri:Uri.c -> ?obj_uri_string:string -> ?obj_blank:string -> ?obj_literal:string -> 
	unit -> object ('self)
	  method repr : repr

	  method is_valid      : bool
	  method to_string     : string
	  method equals        : 'self -> bool
	  method matches       : 'self -> bool
	  method get_subject   : Node.c
	  method get_predicate : Node.c
	  method get_object    : Node.c
	  method set_subject   : Node.c -> unit
	  method set_predicate : Node.c -> unit
	  method set_object    : Node.c -> unit
	  method clone         : 'self
      end
  val from_nodes       : Node.c -> Node.c -> Node.c -> c
  val from_uris        : Uri.c -> Uri.c -> Uri.c -> c
  val from_uri_strings : string -> string -> string -> c
end

(** Provides sequences of statements from parsers and queries. *)
module Stream : sig
(** Redland Stream class *)
  class c : object ('self)
    method repr : repr

    method is_valid : bool
    method context  : Node.c
    method iter     : (Statement.c -> unit) -> unit
  end
  val null : c
end

(** Enumerates nodes from queries. *)
module Iterator : sig
(** Redland Iterator class. *)
  class c : object ('self)
    method repr : repr

    method is_valid : bool
    method iter     : (Node.c -> unit) -> unit
  end
end

(** Storage for models either persistent or in-memory. *)
module Storage : sig
(** Redland Storage class. *)
  class c : ?storage_name:string -> ?name:string -> ?options_string:string -> unit -> object ('self)
    method repr : repr

    method is_valid : bool
    method clone    : 'self
  end
  val dummy     : c
end

(** In-memory storage. *)
module MemoryStorage : sig
  class c : ?name:string -> ?options_string:string -> unit -> object ('self)
    method repr : repr

    method is_valid : bool
    method clone    : 'self
  end
end

(** Hash storage. *)
module HashStorage : sig
  class c : ?name:string -> ?options_string:string -> unit -> object ('self)
    method repr : repr

    method is_valid : bool
    method clone    : 'self
  end
end

(** File storage. *)
module FileStorage : sig
  class c : ?name:string -> ?options_string:string -> unit -> object ('self)
    method repr : repr

    method is_valid : bool
    method clone    : 'self
  end
end

(** Results of querying a model giving either variable bindings 
    with node values or stream of statements. *)
module QueryResults : sig
(** Redland QueryResults class. *)
  class c : object ('self)
    method repr : repr

    method is_valid                  : bool
    method as_stream                 : Stream.c
    method get_count                 : int
    method get_bindings_count        : int      
    method get_binding_name          : int -> string
    method get_binding_value         : int -> Node.c
    method get_binding_value_by_name : string -> Node.c
    method make_assoc                : (string * Node.c) list
    method iter                      : (((string * Node.c) list) -> unit) -> unit
    method is_bindings : bool
    method is_boolean  : bool
    method is_graph    : bool
    method is_syntax   : bool
    method get_boolean : bool
    method to_string   : ?base_uri:Uri.c -> Uri.c -> string
    method to_file     : ?base_uri:Uri.c -> string -> Uri.c -> unit
  end
  val null      : c
end

(** Queries a model delivering a [QueryResults.c] object. *)
module rec Query : sig
(** Redland Query class. *)
  class c : ?language_uri:Uri.c -> ?base_uri:Uri.c -> ?language:string -> string -> object ('self)
    method repr : repr

    method is_valid : bool
    method execute  : Model.c -> QueryResults.c
    method clone    : 'self
  end
end

(** Set of statements usually held in a [Storage.c] object. *)
and Model : sig
(** Redland Model class. *)
  class c : ?options_string:string -> ?options_hash:string -> Storage.c -> object ('self)
    method repr : repr

    method is_valid      : bool
    method size          : int
    method add                            : Node.c -> Node.c -> Node.c -> unit
(* DEPRECATED
    method add_string_literal_statement   : Node.c -> Node.c -> ?xml_language:string -> ?is_wf_xml:int -> string -> unit
    method add_typed_literal_statement    : Node.c -> Node.c -> ?xml_language:string -> ?datatype:Uri.c -> string -> unit
*)
    method add_statement                  : ?context:Node.c -> Statement.c -> unit
    method add_statements                 : ?context:Node.c -> Stream.c -> unit
    method remove_statement               : ?context:Node.c -> Statement.c -> unit
    method remove_statements_with_context : Node.c -> unit
    method contains_statement         : Statement.c -> bool
    method contains_statement_context : Node.c -> bool
    method has_arc_in        : Node.c -> Node.c -> bool
    method has_arc_out       : Node.c -> Node.c -> bool
    method as_stream         : Stream.c
    method as_stream_context : Node.c -> Stream.c
    method find_statements   : Statement.c -> Stream.c
    method get_sources       : Node.c -> Node.c -> Iterator.c
    method get_arcs          : Node.c -> Node.c -> Iterator.c
    method get_arcs_in       : Node.c -> Iterator.c
    method get_arcs_out      : Node.c -> Iterator.c
    method get_targets       : Node.c -> Node.c -> Iterator.c
    method get_source        : Node.c -> Node.c -> Node.c
    method get_arc           : Node.c -> Node.c -> Node.c
    method get_target        : Node.c -> Node.c -> Node.c
    method execute_query     : Query.c -> QueryResults.c
    method sync              : unit
    method load              : ?name:string -> ?mime_type:string -> ?type_uri:Uri.c -> Uri.c -> unit
    method to_string         : ?base_uri:Uri.c -> ?name:string -> ?mime_type:string -> ?type_uri:Uri.c -> unit -> string
    method transaction_start    : unit
    method transaction_commit   : unit
    method transaction_rollback : unit
    method clone : 'self
  end
end

(** RDQL query. *)
module RDQLQuery : sig
  class c : ?base_uri:Uri.c -> string -> object ('self)
    method repr : repr

    method is_valid : bool
    method execute  : Model.c -> QueryResults.c
    method clone    : 'self
  end
end

(** SPARQL query. *)
module SPARQLQuery : sig
  class c : ?base_uri:Uri.c -> string -> object ('self)
    method repr : repr

    method is_valid : bool
    method execute  : Model.c -> QueryResults.c
    method clone    : 'self
  end
end

(** Parses RDF data to deliver stream of statements. *)
module Parser : sig
(** Redland Parser class. *)
  class c : ?name:string -> ?mime_type:string -> ?uri:Uri.c -> unit -> object ('self)
    method repr : repr

    method is_valid                : bool
    method parse_as_stream         : ?base_uri:Uri.c -> Uri.c -> Stream.c
    method parse_into_model        : ?base_uri:Uri.c -> Uri.c -> Model.c -> unit
    method parse_string_as_stream  : ?base_uri:Uri.c -> string -> Stream.c
    method parse_string_into_model : ?base_uri:Uri.c -> string -> Model.c -> unit
    method get_feature             : Uri.c -> Node.c
    method set_feature             : Uri.c -> Node.c -> unit
    method namespaces_seen         : (string * Uri.c) list
  end
end

(** N-Triples parser. *)
module NTriplesParser : sig
  class c : object ('self)
    method repr : repr

    method is_valid : bool
    method parse_as_stream         : ?base_uri:Uri.c -> Uri.c -> Stream.c
    method parse_into_model        : ?base_uri:Uri.c -> Uri.c -> Model.c -> unit
    method parse_string_as_stream  : ?base_uri:Uri.c -> string -> Stream.c
    method parse_string_into_model : ?base_uri:Uri.c -> string -> Model.c -> unit
    method get_feature             : Uri.c -> Node.c
    method set_feature             : Uri.c -> Node.c -> unit
    method namespaces_seen         : (string * Uri.c) list
  end
end

(** Turtle parser. *)
module TurtleParser : sig
  class c : object ('self)
    method repr : repr

    method is_valid                : bool
    method parse_as_stream         : ?base_uri:Uri.c -> Uri.c -> Stream.c
    method parse_into_model        : ?base_uri:Uri.c -> Uri.c -> Model.c -> unit
    method parse_string_as_stream  : ?base_uri:Uri.c -> string -> Stream.c
    method parse_string_into_model : ?base_uri:Uri.c -> string -> Model.c -> unit
    method get_feature             : Uri.c -> Node.c
    method set_feature             : Uri.c -> Node.c -> unit
    method namespaces_seen         : (string * Uri.c) list
  end
end

(** Serializes a model in a format such as RDF/XML. *)
module Serializer : sig
(** Redland Serializer class. *)
  class c : ?name:string -> ?mime_type:string -> ?uri:Uri.c -> unit -> object ('self)
    method repr : repr

    method is_valid                   : bool
    method check_name                 : string -> bool
    method serialize_model_to_file    : ?base_uri:Uri.c -> string -> Model.c -> unit
    method serialize_model_to_string  : ?base_uri:Uri.c -> Model.c -> string
    method serialize_stream_to_file   : ?base_uri:Uri.c -> string -> Stream.c -> unit
    method serialize_stream_to_string : ?base_uri:Uri.c -> Stream.c -> string
    method get_feature                : Uri.c -> Node.c
    method set_feature                : Uri.c -> Node.c -> unit
    method set_namespace              : string -> Uri.c -> unit
  end
end

(** RDF/XML serializer. *)
module RDFXMLSerializer : sig
  class c : object ('self)
    method repr : repr

    method is_valid                   : bool
    method check_name                 : string -> bool
    method serialize_model_to_file    : ?base_uri:Uri.c -> string -> Model.c -> unit
    method serialize_model_to_string  : ?base_uri:Uri.c -> Model.c -> string
    method serialize_stream_to_file   : ?base_uri:Uri.c -> string -> Stream.c -> unit
    method serialize_stream_to_string : ?base_uri:Uri.c -> Stream.c -> string
    method get_feature                : Uri.c -> Node.c
    method set_feature                : Uri.c -> Node.c -> unit
    method set_namespace              : string -> Uri.c -> unit
  end
end

(** N-Triples serializer. *)
module NTriplesSerializer : sig
  class c : object ('self)
    method repr : repr

    method is_valid                   : bool
    method check_name                 : string -> bool
    method serialize_model_to_file    : ?base_uri:Uri.c -> string -> Model.c -> unit
    method serialize_model_to_string  : ?base_uri:Uri.c -> Model.c -> string
    method serialize_stream_to_file   : ?base_uri:Uri.c -> string -> Stream.c -> unit
    method serialize_stream_to_string : ?base_uri:Uri.c -> Stream.c -> string
    method get_feature                : Uri.c -> Node.c
    method set_feature                : Uri.c -> Node.c -> unit
    method set_namespace              : string -> Uri.c -> unit
  end
end
