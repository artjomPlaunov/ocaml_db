module Lru_replacer = Buffer_manager__Lru_replacer

module To_test = struct
  open File

  let lru_eviction1 () =
    let cache = Lru_replacer.make ~capacity_k:2 ~num_buffers:7 in
    assert (Lru_replacer.num_evictable cache = 7);
    assert (Lru_replacer.evict cache = Some 0);
    Lru_replacer.record_access cache 3;
    assert (Lru_replacer.evict cache = Some 0);
    Lru_replacer.record_access cache 2;
    assert (Lru_replacer.evict cache = Some 0);
    Lru_replacer.record_access cache 4;
    assert (Lru_replacer.evict cache = Some 0);
    Lru_replacer.record_access cache 0;
    assert (Lru_replacer.evict cache = Some 1);
    Lru_replacer.record_access cache 1;
    assert (Lru_replacer.evict cache = Some 5);
    assert (Lru_replacer.num_evictable cache = 7)

  let lru_eviction2 () =
    let cache = Lru_replacer.make ~capacity_k:3 ~num_buffers:2 in
    Lru_replacer.record_access cache 1;
    Unix.sleepf 1e-9;
    Lru_replacer.record_access cache 1;
    Unix.sleepf 1e-9;
    Lru_replacer.record_access cache 1;
    Unix.sleepf 1e-9;
    Lru_replacer.record_access cache 0;
    Unix.sleepf 1e-9;
    Lru_replacer.record_access cache 0;
    Unix.sleepf 1e-9;
    assert (Lru_replacer.evict cache = Some 0);
    Lru_replacer.record_access cache 1;
    Unix.sleepf 1e-9;
    assert (Lru_replacer.evict cache = Some 0);
    Lru_replacer.record_access cache 0;
    Unix.sleepf 1e-9;
    assert (Lru_replacer.evict cache = Some 1);
    Lru_replacer.record_access cache 1;
    Unix.sleepf 1e-9;
    assert (Lru_replacer.evict cache = Some 1);
    Lru_replacer.record_access cache 1;
    Unix.sleepf 1e-9;
    assert (Lru_replacer.evict cache = Some 0)
end

let test_lru_eviction1 () =
  Alcotest.(check pass) "just run test lol" () (To_test.lru_eviction1 ())

let test_lru_eviction2 () =
  Alcotest.(check pass) "just run test lol" () (To_test.lru_eviction2 ())

let all_tests () =
  [
    Alcotest.test_case "lru replacer test1" `Quick test_lru_eviction1;
    Alcotest.test_case "lru replacer test2" `Quick test_lru_eviction2;
  ]
