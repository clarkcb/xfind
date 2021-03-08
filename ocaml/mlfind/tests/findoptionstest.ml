open Core.Std
open OUnit

let findoptions = Findoptions.get_findoptions

let test_fixture = "Findoptions" >:::
[
  "test_no_args" >:: (fun () ->
    match (Findoptions.settings_from_args findoptions []) with
    | Ok settings ->
      assert_equal settings.archivesonly false;
      assert_equal settings.debug false;
      assert_equal settings.excludehidden true;
      assert_equal settings.in_archiveextensions [];
      assert_equal settings.in_archivefilepatterns [];
      assert_equal settings.in_dirpatterns [];
      assert_equal settings.in_extensions [];
      assert_equal settings.in_filepatterns [];
      assert_equal settings.includearchives false;
      assert_equal settings.listdirs false;
      assert_equal settings.listfiles true;
      assert_equal settings.out_archiveextensions [];
      assert_equal settings.out_archivefilepatterns [];
      assert_equal settings.out_dirpatterns [];
      assert_equal settings.out_extensions [];
      assert_equal settings.out_filepatterns [];
      assert_equal settings.printusage false;
      assert_equal settings.printversion false;
      assert_equal settings.recursive true;
      assert_equal settings.paths [];
      assert_equal settings.verbose false;
    | _ -> ()
  );

  "test_valid_args" >:: (fun () ->
    let args = ["-x"; "py,rb"; "-s"; "Find"; "."] in
    match (Findoptions.settings_from_args findoptions args) with
    | Ok settings ->
      assert_equal settings.in_extensions ["py"; "rb"];
      assert_equal settings.findpatterns [Re2.Regex.create_exn "Find"];
      assert_equal settings.paths ["."];
    | _ -> ()
  );

  "test_archivesonly_arg" >:: (fun () ->
    let args = ["--archivesonly"] in
    match (Findoptions.settings_from_args findoptions args) with
    | Ok settings ->
      assert_equal settings.archivesonly true;
      assert_equal settings.includearchives true;
    | _ -> ()
  );

  "test_debug_arg" >:: (fun () ->
    let args = ["--debug"] in
    match (Findoptions.settings_from_args findoptions args) with
    | Ok settings ->
      assert_equal settings.debug true;
      assert_equal settings.verbose true;
    | _ -> ()
  );
]

let _ = run_test_tt ~verbose:true test_fixture;;
