(executable(name filer_test)(modules filer_test)(libraries uuidm alcotest alcotest-lwt sxfiler.core sxfiler.domain sxfiler.usecase)(preprocess(pps lwt.ppx)))
(executable(name plan_filer_test)(modules plan_filer_test)(libraries uuidm alcotest alcotest-lwt sxfiler.core sxfiler.domain sxfiler.usecase)(preprocess(pps lwt.ppx)))
(alias(name runtest)(deps filer_test.exe)(action(run %{deps})))
(alias(name runtest)(deps plan_filer_test.exe)(action(run %{deps})))
