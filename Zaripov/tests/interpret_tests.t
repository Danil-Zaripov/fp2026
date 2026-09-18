Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

If you need to put sample program and use it both in your interpreter and preinstalled one,
you could put it into separate file. Thise will need stanza `(cram (deps demo_input.txt))`
in the dune file

  $ ../bin/REPL.exe -cbv -dparsetree <<EOF
  > fun f -> x
  warning: here-document at line 1 delimited by end-of-file (wanted `EOF')
  Parsed result: (Abs (f, (Var x)))
  Evaluated result: (fun f -> x)
  $ ../bin/REPL.exe -dparsetree <<EOF
  > garbage242
  warning: here-document at line 1 delimited by end-of-file (wanted `EOF')
  Parsed result: (App ((Var garbage), (Const 242), []))
  Fatal error: exception Failure("TODO")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Lambda_lib__Lambda.nor_strat.on_app in file "lib/lambda.ml", line 88, characters 17-35
  Called from Dune__exe__REPL.run_single in file "bin/REPL.ml", line 90, characters 17-25
  Called from Dune__exe__REPL in file "bin/REPL.ml", lines 132-139, characters 2-32
  [2]



  $ ../bin/REPL.exe -no -dparsetree <<EOF
  > (fun x -> fun y -> x)(fun u -> u)((fun x -> x x)(fun x -> x x))
  warning: here-document at line 1 delimited by end-of-file (wanted `EOF')
  Parsed result: (App ((Abs (x, (Abs (y, (Var x))))), (Abs (u, (Var u))),
                    [(App ((Abs (x, (App ((Var x), (Var x), [])))),
                        (Abs (x, (App ((Var x), (Var x), [])))), []))
                      ]
                    ))
  Fatal error: exception Failure("TODO")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Dune__exe__REPL.run_single in file "bin/REPL.ml", line 90, characters 17-25
  Called from Dune__exe__REPL in file "bin/REPL.ml", lines 132-139, characters 2-32
  [2]
Below we redirect contents of the file to the evaluator
  $ ../bin/REPL.exe -dparsetree -stop-after parsing   < lam_1+1.txt
  Parsed result: (App (
                    (Abs (m,
                       (Abs (n,
                          (Abs (f,
                             (Abs (x,
                                (App ((Var m), (Var f),
                                   [(Var n); (Var f); (Var x)]))
                                ))
                             ))
                          ))
                       )),
                    (Abs (f, (Abs (x, (App ((Var f), (Var x), [])))))),
                    [(Abs (f, (Abs (x, (App ((Var f), (Var x), []))))))]))

  $ ../bin/REPL.exe -ao   < lam_1+1.txt
  Fatal error: exception Failure("TODO")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Dune__exe__REPL.run_single in file "bin/REPL.ml", line 90, characters 17-25
  Called from Dune__exe__REPL in file "bin/REPL.ml", lines 132-139, characters 2-32
  [2]
  $ ../bin/REPL.exe -ao   < lam_2x1.txt
  Fatal error: exception Failure("TODO")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Dune__exe__REPL.run_single in file "bin/REPL.ml", line 90, characters 17-25
  Called from Dune__exe__REPL in file "bin/REPL.ml", lines 132-139, characters 2-32
  [2]
Call by value doesn't reduce under abstraction
  $ ../bin/REPL.exe -cbv   < lam_2x1.txt
  Fatal error: exception Failure("TODO")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Dune__exe__REPL.run_single in file "bin/REPL.ml", line 90, characters 17-25
  Called from Dune__exe__REPL in file "bin/REPL.ml", lines 132-139, characters 2-32
  [2]
  $ ../bin/REPL.exe -ao -small   < lam_3x2.txt
  Fatal error: exception Failure("TODO")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Dune__exe__REPL.run_single in file "bin/REPL.ml", line 90, characters 17-25
  Called from Dune__exe__REPL in file "bin/REPL.ml", lines 132-139, characters 2-32
  [2]
  $ ../bin/REPL.exe -ao   < lam_zero.txt
  Evaluated result: (fun g -> (fun y -> y))
For 3! we use noral order reduction
  $ cat lam_fac3.txt
  (fun f -> (fun x -> f x x) (fun x -> f x x)) (fun s -> (fun n -> (fun n -> n (fun x -> (fun x -> (fun y -> y))) (fun x -> (fun y -> x))) n (fun f -> (fun x -> f x)) (fun x -> (fun y -> (fun z -> x y z))) s (fun n -> (fun f -> (fun x -> n (fun g -> (fun h -> h g f)) (fun u -> x) (fun u -> u)))) n n)) (fun f -> (fun x -> f f f x))
  $ ../bin/REPL.exe -no   < lam_fac3.txt
  Fatal error: exception Failure("TODO")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Dune__exe__REPL.run_single in file "bin/REPL.ml", line 90, characters 17-25
  Called from Dune__exe__REPL in file "bin/REPL.ml", lines 132-139, characters 2-32
  [2]
