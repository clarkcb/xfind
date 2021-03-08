open Core.Std
open OUnit

let settings = Findsettings.default_settings

let test_fixture = "Finder" >:::
[
  (*****************************************************************************
   * is_find_dir tests
   *****************************************************************************)
  "test_is_find_dir_no_patterns" >:: (fun () ->
    let dir = "mlfind" in
    assert_equal (Finder.is_find_dir settings dir) true;
  );

  "test_is_find_dir_matches_in_pattern" >:: (fun () ->
    let dir = "mlfind" in
    let ss = { settings with in_dirpatterns=[Re2.Regex.create_exn dir] } in
    assert_equal (Finder.is_find_dir ss dir) true;
  );

  "test_is_find_dir_no_match_in_pattern" >:: (fun () ->
    let dir = "mlfind" in
    let ss = { settings with in_dirpatterns=[Re2.Regex.create_exn "nomatch"] } in
    assert_equal (Finder.is_find_dir ss dir) false;
  );

  "test_is_find_dir_matches_out_pattern" >:: (fun () ->
    let dir = "mlfind" in
    let ss = { settings with out_dirpatterns=[Re2.Regex.create_exn dir] } in
    assert_equal (Finder.is_find_dir ss dir) false;
  );

  "test_is_find_dir_no_match_out_pattern" >:: (fun () ->
    let dir = "mlfind" in
    let ss = { settings with out_dirpatterns=[Re2.Regex.create_exn "nomatch"] } in
    assert_equal (Finder.is_find_dir ss dir) true;
  );

  "test_is_find_dir_single_dot" >:: (fun () ->
    let dir = "." in
    assert_equal (Finder.is_find_dir settings dir) true;
  );

  "test_is_find_dir_double_dot" >:: (fun () ->
    let dir = ".." in
    assert_equal (Finder.is_find_dir settings dir) true;
  );

  "test_is_find_dir_hidden_dir" >:: (fun () ->
    let dir = ".git" in
    assert_equal (Finder.is_find_dir settings dir) false;
  );

  "test_is_find_dir_hidden_dir_include_hidden" >:: (fun () ->
    let dir = ".git" in
    let ss = { settings with excludehidden=false } in
    assert_equal (Finder.is_find_dir ss dir) true;
  );

  (*****************************************************************************
   * is_find_file tests
   *****************************************************************************)
  "test_is_find_file_matches_by_default" >:: (fun () ->
    let file = "fileutil.ml" in
    assert_equal (Finder.is_find_file settings file) true;
  );

  "test_is_find_file_matches_in_extension" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with in_extensions=["ml"] } in
    assert_equal (Finder.is_find_file ss file) true;
  );

  "test_is_find_file_no_match_in_extension" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with in_extensions=["pl"] } in
    assert_equal (Finder.is_find_file ss file) false;
  );

  "test_is_find_file_matches_out_extension" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with out_extensions=["ml"] } in
    assert_equal (Finder.is_find_file ss file) false;
  );

  "test_is_find_file_no_match_out_extension" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with out_extensions=["pl"] } in
    assert_equal (Finder.is_find_file ss file) true;
  );

  "test_is_find_file_matches_in_pattern" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with in_filepatterns=[Re2.Regex.create_exn "file"] } in
    assert_equal (Finder.is_find_file ss file) true;
  );

  "test_is_find_file_no_match_in_pattern" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with in_filepatterns=[Re2.Regex.create_exn "find"] } in
    assert_equal (Finder.is_find_file ss file) false;
  );

  "test_is_find_file_matches_out_pattern" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with out_filepatterns=[Re2.Regex.create_exn "file"] } in
    assert_equal (Finder.is_find_file ss file) false;
  );

  "test_is_find_file_no_match_out_pattern" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with out_filepatterns=[Re2.Regex.create_exn "find"] } in
    assert_equal (Finder.is_find_file ss file) true;
  );

  (*****************************************************************************
   * is__archive_find_file tests
   *****************************************************************************)
  "test_is_archive_find_file_matches_by_default" >:: (fun () ->
    let file = "archive.zip" in
    assert_equal (Finder.is_archive_find_file settings file) true;
  );

  "test_is_archive_find_file_matches_in_extension" >:: (fun () ->
    let file = "archive.zip" in
    let ss = { settings with in_archiveextensions=["zip"] } in
    assert_equal (Finder.is_archive_find_file ss file) true;
  );

  "test_is_archive_find_file_no_match_in_extension" >:: (fun () ->
    let file = "archive.zip" in
    let ss = { settings with in_archiveextensions=["gz"] } in
    assert_equal (Finder.is_archive_find_file ss file) false;
  );

  "test_is_archive_find_file_matches_out_extension" >:: (fun () ->
    let file = "archive.zip" in
    let ss = { settings with out_archiveextensions=["zip"] } in
    assert_equal (Finder.is_archive_find_file ss file) false;
  );

  "test_is_archive_find_file_no_match_out_extension" >:: (fun () ->
    let file = "archive.zip" in
    let ss = { settings with out_archiveextensions=["gz"] } in
    assert_equal (Finder.is_archive_find_file ss file) true;
  );

  "test_is_archive_find_file_matches_in_pattern" >:: (fun () ->
    let file = "archive.zip" in
    let ss = { settings with in_archivefilepatterns=[Re2.Regex.create_exn "arch"] } in
    assert_equal (Finder.is_archive_find_file ss file) true;
  );

  "test_is_archive_find_file_no_match_in_pattern" >:: (fun () ->
    let file = "archive.zip" in
    let ss = { settings with in_archivefilepatterns=[Re2.Regex.create_exn "archives"] } in
    assert_equal (Finder.is_archive_find_file ss file) false;
  );

  "test_is_archive_find_file_matches_out_pattern" >:: (fun () ->
    let file = "archive.zip" in
    let ss = { settings with out_archivefilepatterns=[Re2.Regex.create_exn "arch"] } in
    assert_equal (Finder.is_archive_find_file ss file) false;
  );

  "test_is_archive_find_file_no_match_out_pattern" >:: (fun () ->
    let file = "archive.zip" in
    let ss = { settings with out_archivefilepatterns=[Re2.Regex.create_exn "archives"] } in
    assert_equal (Finder.is_archive_find_file ss file) true;
  );

  (*****************************************************************************
   * filter_file tests
   *****************************************************************************)
  "test_filter_file_matches_by_default" >:: (fun () ->
    let file = "fileutil.ml" in
    let sf = Findfile.create file Filetypes.Text in
    assert_equal (Finder.filter_file settings sf) true;
  );

  "test_filter_file_is_find_file" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with in_extensions=["ml"] } in
    let sf = Findfile.create file Filetypes.Text in
    assert_equal (Finder.filter_file ss sf) true;
  );

  "test_filter_file_not_is_find_file" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with in_extensions=["pl"] } in
    let sf = Findfile.create file Filetypes.Text in
    assert_equal (Finder.filter_file ss sf) false;
  );

  "test_filter_file_is_hidden_file" >:: (fun () ->
    let file = ".gitignore" in
    let sf = Findfile.create file Filetypes.Unknown in
    assert_equal (Finder.filter_file settings sf) false;
  );

  "test_filter_file_hidden_includehidden" >:: (fun () ->
    let file = ".gitignore" in
    let ss = { settings with excludehidden=false } in
    let sf = Findfile.create file Filetypes.Unknown in
    assert_equal (Finder.filter_file ss sf) true;
  );

  "test_filter_file_archive_no_includearchives" >:: (fun () ->
    let file = "archive.zip" in
    let sf = Findfile.create file Filetypes.Archive in
    assert_equal (Finder.filter_file settings sf) false;
  );

  "test_filter_file_archive_includearchives" >:: (fun () ->
    let file = "archive.zip" in
    let ss = { settings with includearchives=true } in
    let sf = Findfile.create file Filetypes.Archive in
    assert_equal (Finder.filter_file ss sf) true;
  );

  "test_filter_file_archive_archivesonly" >:: (fun () ->
    let file = "archive.zip" in
    let ss = { settings with includearchives=true; archivesonly=true } in
    let sf = Findfile.create file Filetypes.Archive in
    assert_equal (Finder.filter_file ss sf) true;
  );

  "test_filter_file_nonarchive_archivesonly" >:: (fun () ->
    let file = "fileutil.ml" in
    let ss = { settings with includearchives=true; archivesonly=true } in
    let sf = Findfile.create file Filetypes.Text in
    assert_equal (Finder.filter_file ss sf) false;
  );

]

let _ = run_test_tt ~verbose:true test_fixture;;
