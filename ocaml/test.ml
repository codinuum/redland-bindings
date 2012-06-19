(* test for camlrdf *)

open Rdf

exception Error of string

let test() =
  Printf.printf "***** starting test...\n";

  let storage = new Storage.c ~storage_name:"memory" ~name:"test" () in

  if not storage#is_valid then
    raise (Error "new Rdf.Storage.c failed");

  let model = new Model.c storage in

  if not model#is_valid then
    raise (Error "new Model.c failed");

  let stmt = new Statement.c
      ~subj_uri:(new Uri.c "http://www.dajobe.org/")
      ~pred_uri:(new Uri.c "http://purl.org/dc/elements/1.1/creator")
      ~obj_node:(new Node.c ~literal:"Dave Beckett" ())
      ()
  in

  if not stmt#is_valid then
    raise (Error "new Statement.c failed");

  model#add_statement stmt;

  begin
    Printf.printf "printing all model statements\n";
    let stmt = 
      new Statement.c ~subj_node:Node.null ~pred_node:Node.null ~obj_node:Node.null ()
    in
    (model#find_statements stmt)#iter
      (fun s -> 
	Printf.printf "  found statement: %s\n" s#to_string
      )
  end;

  let parser = new Parser.c ~name:"rdfxml" ~mime_type:"application/rdf+xml" () in

  if not parser#is_valid then
    raise (Error "could not find rdf/xml parser");

  let uri = new Uri.c "file:../data/dc.rdf" in
  Printf.printf "made uri %s\n" uri#to_string;

  (parser#parse_as_stream ~base_uri:uri uri)#iter
    (fun s ->
      Printf.printf "found parsed statement: %s\n" s#to_string;
      model#add_statement s
    );
  
  let rdfxml_string = 
    "<?xml version='1.0'?><rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#' xmlns:dc='http://purl.org/dc/elements/1.1/'><rdf:Description rdf:about='http://www.dajobe.org/' dc:title='Home Page of David Beckett' /></rdf:RDF>"
  in
  (parser#parse_string_as_stream ~base_uri:uri rdfxml_string)#iter
    (fun s ->
      Printf.printf "found parsed statement from string: %s\n" s#to_string;
      model#add_statement s
   );

  begin
    Printf.printf "namespaces seen:\n";
    List.iter 
      (fun (p, u) -> 
	Printf.printf "%s -> %s\n" p u#to_string
      ) parser#namespaces_seen
  end;

  begin
    Printf.printf "adding statements again with model#load \"%s\"\n" uri#to_string;
    model#load uri
  end;

  begin
    Printf.printf "printing model\n";
    model#as_stream#iter
      (fun s ->
	Printf.printf "found statement: %s\n" s#to_string
      )
  end;

  begin
    Printf.printf "searching model by statement\n";
    (model#find_statements 
       (new Statement.c 
	  ~subj_node:Node.null
	  ~pred_uri:(new Uri.c "http://purl.org/dc/elements/1.1/title")
	  ~obj_node:Node.null
	  ()
       ))#iter
      (fun s ->
	Printf.printf "  found statement: %s\n" s#to_string
      )
  end;

  let n1 = new Node.c ~uri_string:"http://www.dajobe.org/" () in
  let n2 = new Node.c ~uri_string:"http://purl.org/dc/elements/1.1/title" () in

  begin
    Printf.printf "searching model for node targets\n";
    (model#get_targets n1 n2)#iter
      (fun n ->
	Printf.printf "  found node: %s\n" n#to_string
      )
  end;

  begin
    Printf.printf "matching statements\n";
    if not
	((new Statement.c ~subj_node:Node.null ~pred_node:Node.null ~obj_node:Node.null ())#matches
	   (new Statement.c ~subj_node:n1 ~pred_node:n2 ~obj_literal:"Title" ()))
    then
      Printf.printf "Failed!\n"
  end;

  begin
    Printf.printf "testing for null\n";
    Printf.printf "%s\n" 
      (new Statement.c ~subj_node:Node.null ~pred_node:Node.null ~obj_node:Node.null ())#get_subject#to_string
  end;

  begin
    Printf.printf "adding datatyped literal statement to model\n";
    try
      model#add
	(new Node.c ~uri_string:"http://example.org/subject" ()) 
	(new Node.c ~uri_string:"http://example.org/predicate" ())
	(new Node.c 
	   ~literal:"Literal content"
	   ~xml_language:"en-GB" 
	   ~datatype:(new Uri.c "http://example.org/datatype") ()
	);
    with
      Failure s -> Printf.printf "Failed! (%s)\n" s
  end;

  begin
    Printf.printf "writing model as RDF/XML to test-out.rdf\n";
    let ser = new RDFXMLSerializer.c in
    ser#serialize_model_to_file "test-out.rdf" model
  end;


  Printf.printf "done\n"
	
let _ =
  test()

