dune clean;
rm -rf tmp_btree_insert_varchar2s/;
rm -rf tmp_btree_insert_parent_root/;
dune exec _build/default/test/run_tests.exe test "Btree";
 cat tmp_btree_insert_varchar2s/test_btree | xxd -c 26;
