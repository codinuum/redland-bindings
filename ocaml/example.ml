(* *)

open Rdf

exception Error of string

let test() =
  Printf.printf "***** starting test...\n";

  let storage = new Storage.c ~storage_name:"hashes" ~name:"test" 
      ~options_string:"new='yes',hash-type='memory',dir='.'" ()
  in
  if not storage#is_valid then
    raise (Error "new Rdf.Storage.c failed");

  let model = new Model.c storage in
  if not model#is_valid then
    raise (Error "new Rdf.Model.c failed");

  let statement = new Statement.c 
      ~subj_uri:(new Uri.c "http://www.dajobe.org/")
      ~pred_uri:(new Uri.c "http://purl.org/dc/elements/1.1/creator")
      ~obj_node:(new Node.c ~literal:"Dave Beckett" ())
      ()
  in
  if not statement#is_valid then
    raise (Error "new Rdf.Statement.c failed");

  model#add_statement statement;

  begin
    (model#find_statements (new Statement.c ()))#iter
      (fun s ->
	Printf.printf "found statement: %s\n" s#to_string
      )
  end;

  begin
    let test_file = "../data/dc.rdf" in
    Printf.printf "parsing Uri (file): %s\n" test_file;
    let uri = new Uri.c ("file:"^test_file) in
    let parser = new Parser.c ~name:"raptor" () in
    if not parser#is_valid then
      raise (Error "failed to create Rdf.Parser.c raptor");

    let count = ref 0 in

    (parser#parse_as_stream ~base_uri:uri uri)#iter
      (fun s ->
	model#add_statement s;
	incr count
      );

    Printf.printf "parsing added %d statements\n" !count
  end;

  begin
    Printf.printf "printing all statements\n";
    model#as_stream#iter
      (fun s ->
	Printf.printf "statement: %s\n" s#to_string
      )
  end;

  begin
    let q = 
      new Query.c "SELECT ?a ?c WHERE (?a dc:title ?c) USING dc FOR <http://purl.org/dc/elements/1.1/>"
    in
    Printf.printf "querying for dc:titles:\n";
   (q#execute model)#iter
      (fun r ->
	Printf.printf "{\n";
	List.iter
	  (fun (k, v) ->
	    Printf.printf "  %s = %s (%s)\n" k v#to_string (Node.type_to_string v#get_type)
	  ) r;
	Printf.printf "}\n";
      );
  end;

  begin
    Printf.printf "writing model to test-out.rdf as rdf/xml\n";
    let serializer = new Serializer.c () in

    if not serializer#is_valid then
      raise (Error "failed to create Rdf.Serialize.c");      

    serializer#set_namespace "dc" (new Uri.c "http://purl.org/dc/elements/1.1/");
    serializer#serialize_model_to_file "test-out.rdf" model;
    Printf.printf "serialized to ntriples as a string size %d bytes" 
      (String.length (model#to_string ~name:"ntriples" ~base_uri:(new Uri.c "http://example.org/base#") ()))
  end;

  Printf.printf "done\n"

let _ = 
  test()
