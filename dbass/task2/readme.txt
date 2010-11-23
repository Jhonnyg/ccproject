------------------------------------------------------------------------------
Databases (HT2010) - Assignment - Task 2
2010-11-23

Sven Andersson, 19860708-4632
Jhonny Göransson, 19840611-8235
------------------------------------------------------------------------------

Oracle L/P: vtda357_014 / pxfpxf

Data Motivation:
We have populated the database with real data taken from the chalmers webpage. 
In order for one student to be able to graduate, almost all of the database 
tables needs to contain atleast a certain amount of data. The final view,
DBCanGraduate, assembles this data based on a series of conjoined SELECT
queries, and determines wether or not a student can graduate based on the
graduation criteria stated on the assignment PM.
With the data currently inserted, one student passes the test and must thus be
eligible to graduate. Cross-referencing the outcome of the student summary
with the database, tells us that the aquired data from the SELECT queries
must be correct.

Files:
	database.sql - Contains SQL to create the tables.
	views.sql - Contains SQL to create the views specified in the assignment.
	data.sql - Contains SQL to propagate the database with some information.

------------------------------------------------------------------------------