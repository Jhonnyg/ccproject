------------------------------------------------------------------------------
Databases (HT2010) - Assignment - Task 1
2010-11-09

Sven Andersson, 19860708-4632
Jhonny Göransson, 19840611-8235
------------------------------------------------------------------------------

When looking at the domain description, we beleive that our relationship model 
is sound. Looking at some of the future possible queries we should be able to support, 
we can see that the relevant information is possible to extract from the diagram.
For example, the domain description tells us that we should be able to see if a 
student is able to graduate depending on a set of requirements. It should be 
possible to see if a student has passed (hasTaken relation) 
the mandatory courses (programme and branchMandatory relations), which could belong 
to a certain class of courses (certifiedAs relation). 

Weak-entity:
We choose branches as weak entities of programmes, as they are unique
depending on the programme they belong to.

ISA:
Since not all courses may have a maximum number of students allowed, we
thought that this will be a good example of a ISA-relation, since a limited-
course would essentially be a sub-entity of Course with one more attribute.

------------------------------------------------------------------------------