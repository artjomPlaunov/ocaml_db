This is an implementation of Simple DB from Edward Sciore's book, "Database Design and Implementation", which has a toy java DB. 

This implements the basic functionalities using basically the same design as the book up through the pipelined query processing chapter. Everything on top of that diverges from the textbook, as we just wanted a very simple base to code with. 

This is meant as a pedagogical tool for people that want to do hacking on a simple database and happen to like writing in OCaml. The main branch will be up to date with all new features, and there will be a separate branch with the initial skeleton for people that want to start from a bare bones base on their own. 

Examples of supported queries (will have a formal grammar here later)

CREATE TABLE t (f1 INT, f2 VARCHAR(10))

SELECT f1, f2, f3 FROM t  WHERE f2 = 5

INSERT INTO t ()



Todo List:
- concurrency control (snapshot isolation)
- b trees, indexing
- query planning/optimization
- (1) clean up code base, solidify tests, try to cook up any fundamental design refactorings before growing the DB further.   
- (2) generate DB docs with odoc. 
- JDBC-like API for network and embedded connection. Test on separate nodes. 
- Transaction + Recovery Manager testing -- shutdown DB instance and test recovery log algo. 
- DB wrapper 
* and especially our modules with the double underscore
* concurrency tests failed with "New value: 679" (have yet to replicate again)
* profiling?
