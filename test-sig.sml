(* A test harness for lab 4 *)
signature TEST =
sig
  (* A type representing a test to run. *)
  type test
  type result = string option

  (* Lists of tests of differents flavors. *)
  
  (* runs a MinML program through the entire procedure:
   * type inference, elaboration, and evaluation *)
  val fullTests : test list
  
  val allTests : test list

  (* given a test and its result, prints a diagnostic *)
  val printTest : (test * result) -> unit

  (* run a specific test and return its result *)
  val doTest : test -> result
  (* do a specific test by list & id# *)
  val doTestN : test list -> int -> result
  (* do many tests *)
  val doTests : test list -> result list

  (* run a specific test and print a diagnostic *)
  val runTest : test -> unit
  (* run a specific test by list & id# *)
  val runTestN : test list -> int -> unit
  (* run many tests and print a summary *)
  val runTests : test list -> unit

  (* run all of the tests *)
  val runAll : unit -> unit

  (* prints information about the specified test *)
  val printInfo : test -> unit
  (* prints information about the specified test by list &id# *)
  val printInfoN : test list -> int -> unit
end
