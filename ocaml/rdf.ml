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

open Printf

let warning mes =
  fprintf stderr "[WARNING] %s\n" mes


exception NodeTypeError of string


type repr = Redland.c_obj

let null_ptr = Swig.C_ptr(0L, 0L)

let is_null_ptr = function
  | Swig.C_ptr(p, _) -> p = 0L
  | _ -> false

let apply2 f (x, y) = f (Swig.C_list [x; y])
let apply3 f (x, y, z) = f (Swig.C_list [x; y; z])
let apply4 f (x, y, z, w) = f (Swig.C_list [x; y; z; w])
let apply5 f (x, y, z, w, a) = f (Swig.C_list [x; y; z; w; a])
let apply6 f (x, y, z, w, a, b) = f (Swig.C_list [x; y; z; w; a; b])


(* World *)
module World = struct

  class c = object (self : 'self)
    val _world = Redland._librdf_new_world Swig.C_void

    method repr = _world

    initializer
      if is_null_ptr _world then
	failwith "Rdf.World.c#initializer"
      else begin
	ignore (Redland._librdf_world_open _world);
	ignore (Redland._librdf_ocaml_world_init _world)
      end;
      Gc.finalise (fun w -> ignore (Redland._librdf_free_world w#repr)) self

  end (* of class Rdf.World.c *)

end (* of module Rdf.World *)

let the_world = new World.c



(* Base class *)
class base = object (self : 'self)

  val mutable _repr = null_ptr

  method repr = _repr

  method is_valid = not self#is_null

  method private is_null = is_null_ptr _repr

  method private set_repr _r =
    if not (is_null_ptr _r) then begin
      _repr <- _r;
    end

  method private register_finalizer (f : repr -> repr) =
    Gc.finalise (fun o -> ignore (f o#repr)) self

  method private get_copied_repr builder =
    if self#is_null then
	_repr
    else
      builder _repr

end (* of class Rdf.base *)



(* URI *)

class _uri_c _r = object (self : 'self)
  inherit base

  method to_string =
    let _s = Redland._librdf_uri_to_string _repr in
    if is_null_ptr _s then
      failwith "Rdf.Uri.c#to_string"
    else
      Swig.get_string _s

  method equals (other : 'self) =
    Swig.get_int (apply2 Redland._librdf_uri_equals(_repr, other#repr)) <> 0

  method clone =
    let _u = self#get_copied_repr Redland._librdf_new_uri_from_uri in
    {< _repr = _u >}

  initializer
    self#register_finalizer Redland._librdf_free_uri;
    self#set_repr _r
end

module Uri = struct

  class c uri_string = 
    let _r = 
      if uri_string <> "" then
	apply2 
	  Redland._librdf_new_uri(the_world#repr, 
				  Swig.make_string uri_string)
      else
	null_ptr
    in
    object (self : 'self)
      inherit _uri_c _r

    end (* of class Uri.c *)

  let null = new c ""

end (* of module Uri *)



(* Node *)

type node_type = T_unknown | T_resource | T_literal | T_blank

type literal_value = {
    lv_string   : string;
    lv_language : string;
    lv_datatype : Uri.c;
  }

let node_type_to_string = function
  | T_unknown  -> "UNKNOWN"
  | T_resource -> "RESOURCE"
  | T_literal  -> "LITERAL"
  | T_blank    -> "BLANK"


class _node_c _r = object (self : 'self)
  inherit base as super

  val mutable node_ty = T_unknown

  method get_type = node_ty

  method is_resource =
    match node_ty with
    | T_resource -> true
    | _ -> false

  method is_literal =
    match node_ty with
    | T_literal -> true
    | _ -> false

  method is_blank =
    match node_ty with
    | T_blank -> true
    | _ -> false

  method is_valid =
    super#is_valid &&
    (match node_ty with
    | T_unknown -> false
    | _ -> true)

  method equals (other : 'self) =
    Swig.get_int (apply2 Redland._librdf_node_equals(_repr, other#repr)) <> 0

  method get_uri =
    if self#is_resource then
      new _uri_c (Redland._librdf_node_get_uri _repr)
    else
      raise (NodeTypeError 
	       (sprintf 
		  "cannot get URI for node type %s" 
		  (node_type_to_string self#get_type)))

  method get_literal_value =
    if not self#is_literal then
      raise (NodeTypeError 
	       (sprintf 
		  "cannot get literal for node type %s" 
		  (node_type_to_string self#get_type)));
    let _dt_uri = 
      Redland._librdf_node_get_literal_value_datatype_uri _repr
    in
    let dt_uri =
      new Uri.c (Swig.get_string (Redland._librdf_uri_to_string _dt_uri))
    in
    { lv_string   = Swig.get_string (Redland._librdf_node_get_literal_value _repr);
      lv_language = Swig.get_string (Redland._librdf_node_get_literal_value_language _repr);
      lv_datatype = dt_uri;
    }

  method get_blank_identifier =
    if self#is_blank then
      Swig.get_string (Redland._librdf_node_get_blank_identifier _repr)
    else
      raise (NodeTypeError 
	       (sprintf 
		  "cannot get blank identifier for node type %s" 
		  (node_type_to_string self#get_type)));

  method to_string =
    if self#is_null then
      "<NULL>"
    else if self#is_literal then
      Swig.get_string (Redland._librdf_node_get_literal_value _repr)
    else if self#is_blank then
      self#get_blank_identifier
    else
      self#get_uri#to_string

  method clone =
    let _n = self#get_copied_repr Redland._librdf_new_node_from_node in
    {< _repr = _n; node_ty = node_ty >}

  method private _get_type =
    if self#is_null then
      T_unknown
    else if Swig.get_int (Redland._librdf_node_is_blank _repr) <> 0 then
      T_blank
    else if Swig.get_int (Redland._librdf_node_is_literal _repr) <> 0 then
      T_literal
    else if Swig.get_int (Redland._librdf_node_is_resource _repr) <> 0 then
      T_resource
    else
      T_unknown

  initializer
    self#register_finalizer Redland._librdf_free_node;
    self#set_repr _r;
    node_ty <- self#_get_type


end (* of class _node_c *)

module Node = struct

  class c
      ?(uri_string="")
      ?(uri=Uri.null)
      ?(literal="")
      ?(datatype=Uri.null)
      ?(is_wf_xml=0)
      ?(xml_language="")
      ?(blank="")
      ()
      = 
    let _r =
      if uri_string <> "" then
	(apply2 
	   Redland._librdf_new_node_from_uri_string(the_world#repr, 
						    Swig.make_string uri_string))
      else if uri != Uri.null then
	(apply2
	   Redland._librdf_new_node_from_uri(the_world#repr, 
					     uri#repr))
      else if literal <> "" then begin
	if datatype != Uri.null && xml_language <> "" then
	  failwith "Rdf.Node.c: optional arguments xml_language and datatype are mutually exclusive"

	else if datatype != Uri.null then
	  (apply4
	     Redland._librdf_new_node_from_typed_literal(the_world#repr, 
							 Swig.make_string literal, 
							 Swig.make_string xml_language, 
							 datatype#repr))
	else
	  (apply4 
	     Redland._librdf_new_node_from_literal(the_world#repr, 
						   Swig.make_string literal, 
						   Swig.make_string xml_language, 
						   Swig.make_int is_wf_xml))
      end
      else if blank <> "" then
	(apply2 
	   Redland._librdf_new_node_from_blank_identifier(the_world#repr, 
							  Swig.make_string blank))
      else
	(Redland._librdf_new_node the_world#repr)
    in
    object (self : 'self)
      inherit _node_c _r

    end (* of class Node.c *)

  let type_to_string = node_type_to_string

  let from_uri_string s = new c ~uri_string:s ()
  let from_uri u = new c ~uri:u ()

  let from_literal ?(xml_language="") ?(is_wf_xml=0) s = 
    new c ~literal:s ~xml_language ~is_wf_xml ()

  let from_typed_literal ?(xml_language="") ?(datatype=Uri.null) s =
    new c ~literal:s ~xml_language ~datatype ()

  let from_blank s = new c ~blank:s ()

  let null = new _node_c null_ptr

end (* of module Node *)



(* Statement *)

class _statement_c _r = object (self : 'self)
  inherit base

  method to_string =
    if not self#is_valid then
      failwith "Rdf.Statement.c#to_string"
    else
      Swig.get_string (Redland._librdf_statement_to_string _repr)

  method equals (other : 'self) =
    Swig.get_int (apply2 Redland._librdf_statement_equals(_repr, other#repr)) <> 0

  method matches (other : 'self) =
    Swig.get_int (apply2 Redland._librdf_statement_match(other#repr, _repr)) <> 0

  method get_subject =
    new _node_c (Redland._librdf_statement_get_subject _repr)

  method get_predicate =
    new _node_c (Redland._librdf_statement_get_predicate _repr)

  method get_object =
    new _node_c (Redland._librdf_statement_get_object _repr)

  method set_subject (n : Node.c) =
    let _n = Redland._librdf_new_node_from_node n#repr in
    ignore (apply2 Redland._librdf_statement_set_subject(_repr, _n))

  method set_predicate (n : Node.c) =
    let _n = Redland._librdf_new_node_from_node n#repr in
    ignore (apply2 Redland._librdf_statement_set_predicate(_repr, _n))

  method set_object (n : Node.c) =
    let _n = Redland._librdf_new_node_from_node n#repr in
    ignore (apply2 Redland._librdf_statement_set_object(_repr, _n))

  method clone =
    let _s = self#get_copied_repr Redland._librdf_new_statement_from_statement in
    {< _repr = _s >}

  initializer
    self#register_finalizer Redland._librdf_free_statement;
    self#set_repr _r

  end (* of class _statement_c *)


module Statement = struct

  class c
      ?(subj_node=Node.null)
      ?(subj_uri=Uri.null)
      ?(subj_uri_string="")
      ?(subj_blank="")

      ?(pred_node=Node.null)
      ?(pred_uri=Uri.null)
      ?(pred_uri_string="")

      ?(obj_node=Node.null)
      ?(obj_uri=Uri.null)
      ?(obj_uri_string="")
      ?(obj_blank="")
      ?(obj_literal="")
      
      ()
      =
    let _r =
      let subj_node_flag       = subj_node != Node.null in
      let subj_uri_flag        = subj_uri != Uri.null in
      let subj_uri_string_flag = subj_uri_string <> "" in
      let subj_blank_flag      = subj_blank <> "" in
      let pred_node_flag       = pred_node != Node.null in
      let pred_uri_flag        = pred_uri != Uri.null in
      let pred_uri_string_flag = pred_uri_string <> "" in
      let obj_node_flag        = obj_node != Node.null in
      let obj_uri_flag         = obj_uri != Uri.null in
      let obj_uri_string_flag  = obj_uri_string <> "" in
      let obj_blank_flag       = obj_blank <> "" in
      let obj_literal_flag     = obj_blank <> "" in

      let subj_count = ref 0 in
      let pred_count = ref 0 in
      let obj_count  = ref 0 in

      if subj_node_flag then incr subj_count;
      if subj_uri_flag then incr subj_count;
      if subj_uri_string_flag then incr subj_count;
      if subj_blank_flag then incr subj_count;
      if pred_node_flag then incr pred_count;
      if pred_uri_flag then incr pred_count;
      if pred_uri_string_flag then incr pred_count;
      if obj_node_flag then incr obj_count;
      if obj_uri_flag then incr obj_count;
      if obj_uri_string_flag then incr obj_count;
      if obj_blank_flag then incr obj_count;
      if obj_literal_flag then incr obj_count;

      if !subj_count > 1 then
	warning "Rdf.Statement.c#initializer: multiple subjects specified";
      if !pred_count > 1 then
	warning "Rdf.Statement.c#initializer: multiple predicates specified";
      if !obj_count > 1 then
	warning "Rdf.Statement.c#initizlizer: multiple objects specified";

      let s =
	if subj_node_flag then
	  subj_node#clone

	else if subj_uri_flag then
	  Node.from_uri subj_uri

	else if subj_uri_string_flag then
	  Node.from_uri_string subj_uri_string

	else  if subj_blank_flag then
	  Node.from_blank subj_blank

	else
	  Node.null
      in

      let p =
	if pred_node_flag then
	  pred_node#clone

	else if pred_uri_flag then
	  Node.from_uri pred_uri

	else if pred_uri_string_flag then
	  Node.from_uri_string pred_uri_string

	else 
	  Node.null
      in

      let o =
	if obj_node_flag then
	  obj_node#clone

	else if obj_uri_flag then
	  Node.from_uri obj_uri

	else if obj_uri_string_flag then
	  Node.from_uri_string obj_uri_string

	else  if obj_blank_flag then
	  Node.from_blank obj_blank

	else if obj_literal_flag then
	  Node.from_literal obj_literal

	else
	  Node.null
      in
      
      (apply4
	 Redland._librdf_new_statement_from_nodes(the_world#repr, 
						  s#repr, 
						  p#repr, 
						  o#repr))
    in
    object (self : 'self)
      inherit _statement_c _r

    end (* of class Statement.c *)

  let from_nodes sn pn on =
    new c ~subj_node:sn ~pred_node:pn ~obj_node:on ()

  let from_uris su pu ou =
    new c ~subj_uri:su ~pred_uri:pu ~obj_uri:ou ()

  let from_uri_strings ss ps os =
    new c ~subj_uri_string:ss ~pred_uri_string:ps ~obj_uri_string:os ()

end (* of module Statement *)


(* Stream *)

class _stream_c _r = object (self : 'self)
  inherit base

  method context = 
    new _node_c (Redland._librdf_stream_get_context _repr)

  method private current = 
    new _statement_c (Redland._librdf_stream_get_object _repr)

  method private end_ = 
    Swig.get_int (Redland._librdf_stream_end _repr)

  method private next = 
    Swig.get_int (Redland._librdf_stream_next _repr)

  method iter (f : Statement.c -> unit) =
    if not self#is_valid then
      failwith "Rdf.Stream.c#iter";
    try
      while true do
	f (self#current);
	if self#next <> 0 then
	  raise Exit
      done
    with 
      Exit -> ()

  initializer
    self#register_finalizer Redland._librdf_free_stream;
    self#set_repr _r

end (* of class _stream_c *)


module Stream = struct

  class c = object
    inherit _stream_c null_ptr
  end

  let null = new c

end (* of module Stream *)


(* Iterator *)

class _iterator_c _r = object (self : 'self)
  inherit base

  method private end_ = 
    Swig.get_int (Redland._librdf_iterator_end _repr)

  method private next = 
    Swig.get_int (Redland._librdf_iterator_next _repr)

  initializer
    self#register_finalizer Redland._librdf_free_iterator;
    self#set_repr _r

end (* of class _iterator_c *)

(*
module Iterator = struct
  class c = object
    inherit _iterator_c null_ptr
  end
end (* of module Rdf.Iterator *)
*)

class _node_iterator_c _r = object (self : 'self)
  inherit _iterator_c _r

  method private current = 
    new _node_c (Redland._librdf_iterator_get_object _repr)

  method iter (f : Node.c -> unit) =
    if not self#is_valid then
      failwith "Rdf.Iterator.c#iter";
    try
      while true do
	f (self#current);
	if self#next <> 0 then
	  raise Exit
      done
    with Exit -> ()
	
end (* of class _node_iterator_c *)

module Iterator = struct
  class c = object
    inherit _node_iterator_c null_ptr
  end
end (* of module Iterator *)


(* Storage *)
module Storage = struct

  class c
      ?(storage_name="")
      ?(name="")
      ?(options_string="")
      ()
      =
    object (self : 'self)
      inherit base

      method clone =
	let _s = self#get_copied_repr Redland._librdf_new_storage_from_storage in
	{< _repr = _s >}

      initializer
	self#register_finalizer Redland._librdf_free_storage;

	if storage_name <> "" then
	  self#set_repr
	    (apply4
	       Redland._librdf_new_storage(the_world#repr, 
					   Swig.make_string storage_name, 
					   Swig.make_string name, 
					   Swig.make_string options_string))
	else
	  self#set_repr 
	    (apply4
	       Redland._librdf_new_storage(the_world#repr, 
					   Swig.make_string "memory", 
					   Swig.make_string "", 
					   Swig.make_string ""))

    end (* of class Storage.c *)

  let dummy = new c ()

end (* of module Storage *)

module MemoryStorage = struct
  class c ?(name="") ?(options_string="") () = object
    inherit Storage.c ~storage_name:"memory" ~name ~options_string ()
  end
end (* of module MemoryStorage *)

module HashStorage = struct
  class c ?(name="") ?(options_string="") () = object
    inherit Storage.c ~storage_name:"hashes" ~name ~options_string ()
  end
end (* of module HashStorage *)

module FileStorage = struct
  class c ?(name="") ?(options_string="") () = object
    inherit Storage.c ~storage_name:"file" ~name ~options_string ()
  end
end (* of module FileStorage *)


(* QueryResults *)

class _query_results_c _r = object (self : 'self)
  inherit base

  method as_stream =
    let _s = Redland._librdf_query_results_as_stream _repr in
    if is_null_ptr _s then
      failwith "Rdf.QueryResults.c#as_stream"
    else
      (new _stream_c _s : Stream.c)

  method get_count =
    Swig.get_int (Redland._librdf_query_results_get_count _repr)

  method private next =
    Swig.get_int (Redland._librdf_query_results_next _repr)

  method private finished =
    Swig.get_int (Redland._librdf_query_results_finished _repr)

  method get_bindings_count =
    Swig.get_int (Redland._librdf_query_results_get_bindings_count _repr)

  method get_binding_name offset =
    let _s =
      apply2 Redland._librdf_query_results_get_binding_name(_repr, Swig.make_int offset)
    in
    if is_null_ptr _s then
      failwith "Rdf.QueryResults.c#get_binding_name"
    else
      Swig.get_string _s

  method get_binding_value offset =
    let _n = 
      apply2 Redland._librdf_query_results_get_binding_value(_repr, Swig.make_int offset)
    in
    if is_null_ptr _n then
      failwith "Rdf.QueryResults.c#get_binding_value"
    else
      (new _node_c _n : Node.c)

  method get_binding_value_by_name name =
    let _n =
      apply2 
	Redland._librdf_query_results_get_binding_value_by_name(_repr, 
								Swig.make_string name)
    in
    if is_null_ptr _n then
      failwith "Rdf.QueryResults.c#get_binding_value_by_name"
    else
      (new _node_c _n : Node.c)

  method make_assoc =
    let r = ref [] in
    let c = self#get_bindings_count in
    for i = 0 to c - 1 do
      let n = self#get_binding_name i in
      let v = self#get_binding_value i in
      r := (n, v) :: !r
    done;
    List.rev !r

  method iter (f : ((string * Node.c) list) -> unit) =
    if not self#is_valid then
      failwith "Rdf.QueryResults.c#iter"

    else if not self#is_bindings then 
      failwith "Rdf.QueryResults.c#iter";

    try
      while true do
	f (self#make_assoc);
	if self#next <> 0 then
	  raise Exit
      done
    with 
      Exit -> ()

  method is_bindings =
    Swig.get_int (Redland._librdf_query_results_is_bindings _repr) <> 0

  method is_boolean =
    Swig.get_int (Redland._librdf_query_results_is_boolean _repr) <> 0

  method is_graph =
    Swig.get_int (Redland._librdf_query_results_is_graph _repr) <> 0

  method is_syntax =
    Swig.get_int (Redland._librdf_query_results_is_syntax _repr) <> 0

  method get_boolean =
    let n = 
      Swig.get_int (Redland._librdf_query_results_is_boolean _repr) 
    in
    if n > 0 then
      true
    else if n = 0 then
      false
    else
      raise Exit

  method to_string ?(base_uri=Uri.null) (format_uri : Uri.c) =
    let _s = 
      apply3 Redland._librdf_query_results_to_string(_repr, format_uri#repr, base_uri#repr)
    in
    if is_null_ptr _s then
      failwith "Rdf.QueryResults.c#to_string"
    else
      Swig.get_string _s

  method to_file ?(base_uri=Uri.null) name (format_uri : Uri.c) =
    let ret = 
      Swig.get_int
	(apply4 
	   Redland._librdf_query_results_to_string(_repr, 
						   Swig.make_string name, 
						   format_uri#repr, 
						   base_uri#repr))
    in
    if ret <> 0 then
      failwith "Rdf.QueryResults.c#to_string"

  initializer
    self#register_finalizer Redland._librdf_free_query_results;
    self#set_repr _r

end (* of class _query_results_c *)


module QueryResults = struct

  class c = object
    inherit _query_results_c null_ptr
  end

  let null = new c

end (* of module QueryResults *)



(* Query *)

module rec Query : sig
  class c : ?language_uri:Uri.c -> ?base_uri:Uri.c -> ?language:string -> string -> object ('self)
    method repr : repr

    method is_valid : bool
    method execute  : Model.c -> QueryResults.c
    method clone    : 'self
  end
end = struct

  class c 
      ?(language_uri=Uri.null)
      ?(base_uri=Uri.null)
      ?(language="rdql")
      query_string
      = 
    object (self : 'self)
      inherit base

      method execute (model : Model.c) =
	let _r = apply2 Redland._librdf_query_execute(_repr, model#repr) in
	if is_null_ptr _r then
	  failwith "Rdf.Query.c#execute"
	else
	  (new _query_results_c _r : QueryResults.c)

      method clone =
	let _q = self#get_copied_repr Redland._librdf_new_query_from_query in
	{< _repr = _q >}

      initializer
	self#register_finalizer Redland._librdf_free_query;
	self#set_repr 
	  (apply5 
	     Redland._librdf_new_query(the_world#repr,
				       Swig.make_string language,
				       language_uri#repr,
				       Swig.make_string query_string,
				       base_uri#repr))

    end (* of class Query.c *)

end (* of module Query *)


(* Model *)
and Model : sig
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
end = struct
  
  class c 
      ?(options_string="")
      ?(options_hash="")
      storage
      = 
    object (self : 'self)
      inherit base

      method size =
	Swig.get_int (Redland._librdf_model_size _repr)

      method add (s : Node.c) (p : Node.c) (o : Node.c) =
	if not s#is_valid then
	  raise (Invalid_argument "Rdf.Model.c#add: invalid subject");
	if not p#is_valid then
	  raise (Invalid_argument "Rdf.Model.c#add: invalid predicate");
	if not o#is_valid then
	  raise (Invalid_argument "Rdf.Model.c#add: invalid object");

	if
	  Swig.get_int
	    (apply4
	       Redland._librdf_model_add(_repr, 
					 Redland._librdf_new_node_from_node s#repr,
					 Redland._librdf_new_node_from_node p#repr,
					 Redland._librdf_new_node_from_node o#repr))
	    <> 0
	then
	  failwith "Rdf.Model.c#add"

(* DEPRECATED
      method add_string_literal_statement 
	  (s : Node.c) (p : Node.c) ?(xml_language="") ?(is_wf_xml=0) lit =
	let _lang = 
	  if xml_language = "" then 
	    null_ptr 
	  else 
	    Swig.make_string xml_language 
	in
	if
	  Swig.get_int
	    (apply6
	       Redland._librdf_model_add_string_literal_statement(_repr,
								  Redland._librdf_new_node_from_node s#repr,
								  Redland._librdf_new_node_from_node p#repr,
								  Swig.make_string lit,
								  _lang,
								  Swig.make_int is_wf_xml))
	    <> 0
	then
	  failwith "Rdf.Model.c#add_string_literal_statement"

      method add_typed_literal_statement
	  (s : Node.c) (p : Node.c) ?(xml_language="") ?(datatype=Uri.null) lit =
	let _lang = 
	  if xml_language = "" then 
	    null_ptr 
	  else 
	    Swig.make_string xml_language 
	in
	let ret = 
	  Swig.get_int
	    (apply6
	       Redland._librdf_model_add_typed_literal_statement(_repr,
								 Redland._librdf_new_node_from_node s#repr,
								 Redland._librdf_new_node_from_node p#repr,
								 Swig.make_string lit,
								 _lang,
								 datatype#repr))
	in
	if ret <> 0 then
	  failwith "Rdf.Model.c#add_typed_literal_statement"
*)
      method add_statement ?(context=Node.null) (stmt : Statement.c) =
	let ret =
	  if context == Node.null then
	    Swig.get_int 
	      (apply2 Redland._librdf_model_add_statement(_repr, stmt#repr)) 
	else
	  Swig.get_int 
	    (apply3 Redland._librdf_model_context_add_statement(_repr, context#repr, stmt#repr)) 
	in
	if ret <> 0 then
	  failwith "Rdf.Model.c#add_statement"

      method add_statements ?(context=Node.null) (strm : Stream.c) =
	let ret = 
	  if context == Node.null then
	    Swig.get_int 
	      (apply2 Redland._librdf_model_add_statements(_repr, strm#repr)) 
	  else
	    Swig.get_int 
	      (apply3 Redland._librdf_model_context_add_statements(_repr, context#repr, strm#repr)) 
	in
	if ret <> 0 then
	  failwith "Rdf.Model.c#add_statements"

      method remove_statement ?(context=Node.null) (stmt : Statement.c) =
	let ret = 
	  if context == Node.null then
	    Swig.get_int 
	      (apply2 Redland._librdf_model_remove_statement(_repr, stmt#repr)) 
	  else
	    Swig.get_int 
	      (apply3 Redland._librdf_model_context_remove_statement(_repr, context#repr, stmt#repr)) 
	in
	if ret <> 0 then
	  failwith "Rdf.Model.c#remove_statement"

      method remove_statements_with_context (context : Node.c) =
	if
	  Swig.get_int
	    (apply2 Redland._librdf_model_context_remove_statements(_repr, context#repr))
	    <> 0
	then
	  failwith "Rdf.Model.c#remove_statements_with_context"

      method contains_statement (stmt : Statement.c) =
	let n = 
	  Swig.get_int (apply2 Redland._librdf_model_contains_statement(_repr, stmt#repr)) 
	in
	if n > 0 then
	  true
	else if n = 0 then
	  false
	else
	  raise (Invalid_argument "Rdf.Model.c#contains_statement")

      method contains_statement_context (context : Node.c) =
	Swig.get_int (apply2 Redland._librdf_model_contains_context(_repr, context#repr)) = 0
	  
      method has_arc_in (n : Node.c) (p : Node.c) =
	Swig.get_int (apply3 Redland._librdf_model_has_arc_in(_repr, n#repr, p#repr)) = 0

      method has_arc_out (n : Node.c) (p : Node.c) =
	Swig.get_int (apply3 Redland._librdf_model_has_arc_out(_repr, n#repr, p#repr)) = 0

      method as_stream =
	let _s = Redland._librdf_model_as_stream _repr in
	if is_null_ptr _s then
	  failwith "Rdf.Model.c#as_stream"
	else
	  (new _stream_c _s : Stream.c)

      method as_stream_context (context : Node.c) =
	let _s =
	  apply2 Redland._librdf_model_context_as_stream(_repr, context#repr)
	in
	if is_null_ptr _s then
	  failwith "Rdf.Model.c#as_stream_context"
	else
	  (new _stream_c _s : Stream.c)

      method find_statements (stmt : Statement.c) =
	let _s = apply2 Redland._librdf_model_find_statements(_repr, stmt#repr) in
	if is_null_ptr _s then
	  failwith "Rdf.Model.c#find_statements"
	else
	  (new _stream_c _s : Stream.c)

      method get_sources (arc : Node.c) (target : Node.c) =
	(new _node_iterator_c
	  (apply3 Redland._librdf_model_get_sources(_repr, arc#repr, target#repr)) : Iterator.c)

      method get_arcs (src : Node.c) (target : Node.c) =
	(new _node_iterator_c
	  (apply3 Redland._librdf_model_get_arcs(_repr, src#repr, target#repr)) : Iterator.c)

      method get_targets (src : Node.c) (arc : Node.c) =
	(new _node_iterator_c
	  (apply3 Redland._librdf_model_get_targets(_repr, src#repr, arc#repr)) : Iterator.c)

      method get_source (arc : Node.c) (target : Node.c) =
	new _node_c (apply3 Redland._librdf_model_get_source(_repr, arc#repr, target#repr))

      method get_arc (src : Node.c) (target : Node.c) =
	new _node_c (apply3 Redland._librdf_model_get_arc(_repr, src#repr, target#repr))

      method get_target (src : Node.c) (arc : Node.c) =
	new _node_c (apply3 Redland._librdf_model_get_target(_repr, src#repr, arc#repr))
	  
      method get_arcs_in (n : Node.c) =
	(new _node_iterator_c
	  (apply2 Redland._librdf_model_get_arcs_in(_repr, n#repr)) : Iterator.c)

      method get_arcs_out (n : Node.c) =
	(new _node_iterator_c
	  (apply2 Redland._librdf_model_get_arcs_out(_repr, n#repr)) : Iterator.c)

      method execute_query (q : Query.c) =
	(new _query_results_c (apply2 Redland._librdf_model_query_execute(_repr, q#repr)) : QueryResults.c)

      method sync =
	if Swig.get_int (Redland._librdf_model_sync _repr) <> 0 then
	  failwith "Rdf.Model.c#sync"

      method load ?(name="") ?(mime_type="") ?(type_uri=Uri.null) (uri : Uri.c) =
	let _name = 
	  if name = "" then 
	    null_ptr 
	  else 
	    Swig.make_string name 
	in
	let _mime_type = 
	  if mime_type = "" then 
	    null_ptr 
	  else 
	    Swig.make_string mime_type 
	in
	let ret = 
	  Swig.get_int
	    (apply5 Redland._librdf_model_load(_repr, uri#repr, _name, _mime_type, type_uri#repr))
	in
	if ret <> 0 then
	  failwith "Rdf.Model.c#load"

      method to_string ?(base_uri=Uri.null) ?(name="") ?(mime_type="") ?(type_uri=Uri.null) () =
	let _name =
	  if name = "" then
	    null_ptr
	  else
	    Swig.make_string name
	in
	let _mime_type =
	  if mime_type = "" then
	    null_ptr
	  else
	    Swig.make_string mime_type
	in
	let _s =
	  apply5 Redland._librdf_model_to_string(_repr, base_uri#repr, _name, _mime_type, type_uri#repr)
	in
	if is_null_ptr _s then
	  failwith "Rdf.Model.c#to_string"
	else
	  Swig.get_string _s

      method transaction_start =
	let ret = Swig.get_int (Redland._librdf_model_transaction_start _repr) in
	if ret <> 0 then
	  failwith "Rdf.Model.c#transaction_start"

      method transaction_commit =
	let ret = Swig.get_int (Redland._librdf_model_transaction_commit _repr) in
	if ret <> 0 then
	  failwith "Rdf.Model.c#transaction_commit"
	
      method transaction_rollback =
	let ret = Swig.get_int (Redland._librdf_model_transaction_rollback _repr) in
	if ret <> 0 then
	  failwith "Rdf.Model.c#transaction_rollback"

      method clone =
	let _m = self#get_copied_repr Redland._librdf_new_model_from_model in
	{< _repr = _m >}

      initializer
	self#register_finalizer Redland._librdf_free_model;
	self#set_repr
	  (apply3
	     Redland._librdf_new_model(the_world#repr, 
				       storage#repr, 
				       Swig.make_string ""))

    end (* of class Model.c *)

end (* of module Model *)

module RDQLQuery = struct
  class c ?(base_uri=Uri.null) query_string = object
    inherit Query.c ~base_uri ~language:"rdql" query_string
  end
end

module SPARQLQuery = struct
  class c ?(base_uri=Uri.null) query_string = object
    inherit Query.c ~base_uri ~language:"sparql" query_string
  end
end


(* Parser *)
module Parser = struct

  class c 
      ?(name="") 
      ?(mime_type="") 
      ?(uri=Uri.null) 
      ()
      = 
    object (self : 'self)
      inherit base

      method parse_as_stream ?(base_uri=Uri.null) (uri : Uri.c) =
	(new _stream_c
	  (apply3 
	     Redland._librdf_parser_parse_as_stream(_repr, uri#repr, base_uri#repr)) : Stream.c)

      method parse_into_model ?(base_uri=Uri.null) (uri : Uri.c) (model : Model.c) =
	if
	  Swig.get_int
	    (apply4 
	       Redland._librdf_parser_parse_into_model(_repr, 
						       uri#repr, 
						       base_uri#repr, 
						       model#repr))
	    <> 0
	then
	  failwith "Rdf.Parser.c#parse_into_model"

      method parse_string_as_stream ?(base_uri=Uri.null) str =
	(new _stream_c
	  (apply3 
	     Redland._librdf_parser_parse_string_as_stream(_repr, 
							   Swig.make_string str, 
							   base_uri#repr)) : Stream.c)
	  
      method parse_string_into_model ?(base_uri=Uri.null) str (model : Model.c) =
	if
	  Swig.get_int
	    (apply4 
	       Redland._librdf_parser_parse_into_model(_repr, 
						       Swig.make_string str, 
						       base_uri#repr, 
						       model#repr))
	    <> 0
	then
	  failwith "Rdf.Parser.c#parse_string_into_model"

      method get_feature (feature : Uri.c) =
	(new _node_c 
	  (apply2 Redland._librdf_parser_get_feature(_repr, feature#repr)) : Node.c)

      method set_feature (feature : Uri.c) (value : Node.c) =
	if
	  Swig.get_int 
	    (apply3 
	       Redland._librdf_parser_set_feature(_repr, feature#repr, value#repr))
	    <> 0
	then
	  failwith "Rdf.Parser.c#set_feature"

      method namespaces_seen =
	let count = 
	  Swig.get_int (Redland._librdf_parser_get_namespaces_seen_count _repr)
	in
	let nspaces = ref [] in
	for idx = 0 to count - 2 do
	  let prefix = 
	    Swig.get_string
	      (apply2 
		 Redland._librdf_parser_get_namespaces_seen_prefix(_repr, Swig.make_int idx))
	  in
	  let uri =
	    new _uri_c
	      (apply2 
		 Redland._librdf_parser_get_namespaces_seen_uri(_repr, Swig.make_int idx))
	  in
	  nspaces := (prefix, uri) :: !nspaces
	done;
	!nspaces

      initializer
	self#register_finalizer Redland._librdf_free_parser;
	let _name =
	  if name = "" then
	    null_ptr
	  else
	    Swig.make_string name
	in
	let _mime_type =
	  if mime_type = "" then
	    null_ptr
	  else
	    Swig.make_string mime_type
	in
	self#set_repr 
	  (apply4
	     Redland._librdf_new_parser(the_world#repr, _name, _mime_type, uri#repr))

    end (* of class Parser.c *)

end (* of module Parser *)

module NTriplesParser = struct
  class c = object
    inherit Parser.c ~name:"ntriples" ()
  end
end

module TurtleParser = struct
  class c = object
    inherit Parser.c ~name:"turtle" ()
  end
end


(* Serializer *)
module Serializer = struct

  class c
      ?(name="") 
      ?(mime_type="") 
      ?(uri=Uri.null) 
      ()
      = 
    object (self : 'self)
      inherit base

      method check_name name =
	Swig.get_int
	  (apply2
	     Redland._librdf_serializer_check_name(the_world#repr, Swig.make_string name))
	  <> 0

      method serialize_model_to_file ?(base_uri=Uri.null) name (model : Model.c) =
	if
	  Swig.get_int
	    (apply4 
	       Redland._librdf_serializer_serialize_model_to_file(_repr,
								  Swig.make_string name,
								  base_uri#repr,
								  model#repr))
	    <> 0
	then
	  failwith "Rdf.Serializer.c#serialize_model_to_file"

      method serialize_model_to_string ?(base_uri=Uri.null) (model : Model.c) =
	Swig.get_string
	  (apply3
	     Redland._librdf_serializer_serialize_model_to_file(_repr,
								base_uri#repr,
								model#repr))

      method serialize_stream_to_file ?(base_uri=Uri.null) name (stream : Stream.c) =
	if
	  Swig.get_int
	    (apply4 
	       Redland._librdf_serializer_serialize_stream_to_file(_repr,
								   Swig.make_string name,
								   base_uri#repr,
								   stream#repr))
	    <> 0
	then
	  failwith "Rdf.Serializer.c#serialize_stream_to_file"

      method serialize_stream_to_string ?(base_uri=Uri.null) (stream : Stream.c) =
	Swig.get_string
	  (apply3
	     Redland._librdf_serializer_serialize_stream_to_file(_repr,
								 base_uri#repr,
								 stream#repr))

      method get_feature (feature : Uri.c) =
	new _node_c 
	  (apply2 Redland._librdf_serializer_get_feature(_repr, feature#repr))

      method set_feature (feature : Uri.c) (value : Node.c) =
	if
	  Swig.get_int 
	    (apply3 
	       Redland._librdf_serializer_set_feature(_repr, feature#repr, value#repr))
	    <> 0
	then
	  failwith "Rdf.Serializer.c#set_feature"

      method set_namespace prefix (uri : Uri.c) =
	let _prefix = 
	  if prefix = "" then
	    null_ptr
	  else
	    Swig.make_string prefix
	in
	if
	  Swig.get_int 
	    (apply3 
	       Redland._librdf_serializer_set_namespace(_repr, uri#repr, _prefix))
	    <> 0
	then
	  failwith "Rdf.Serializer.c#set_namespace"

      initializer
	self#register_finalizer Redland._librdf_free_serializer;

	let _name =
	  if name = "" then
	    null_ptr
	  else
	    Swig.make_string name
	in
	let _mime_type =
	  if mime_type = "" then
	    null_ptr
	  else
	    Swig.make_string mime_type
	in
	self#set_repr
	  (apply4
	     Redland._librdf_new_serializer(the_world#repr, _name, _mime_type, uri#repr))

    end (* of class Selializer.c *)

end (* of module Serializer *)

module RDFXMLSerializer = struct
  class c = object
    inherit Serializer.c ~name:"rdfxml" ~mime_type:"application/rdf+xml" ()
  end
end

module NTriplesSerializer = struct
  class c = object
      inherit Serializer.c ~name:"ntriples" ()
  end
end
