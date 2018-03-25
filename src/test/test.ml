let _ =
  Alcotest.run "Animation" [
    "client_server",        Test_cs.test_set;
    ]

