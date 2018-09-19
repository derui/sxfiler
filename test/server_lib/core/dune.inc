(executable(name workbench_state_test)(modules workbench_state_test)(libraries alcotest alcotest-lwt sxfiler.server.core)(flags -safe-string)(preprocess(pps lwt.ppx)))
(alias(name runtest)(deps workbench_state_test.exe)(action(run %{deps})))
