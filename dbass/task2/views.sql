CREATE VIEW DBStudents AS
	SELECT Student.name as name,programme,branch 
	FROM Student,Branch
	WHERE programme = Branch.progname;

CREATE VIEW DBFinishedCourses AS 
	SELECT Student.name as name,Course.name as coursename, grade
	FROM HasTaken,Student,Course
	WHERE Student.persnumber = HasTaken.persnumber
	AND Course.code = HasTaken.code;
	


--For all students, the mandatory courses (branch and programme) they have not yet taken.
/*
SELECT Student.name as name,Course.name as coursename
FROM Student,BranchMandatory,ProgrammeMandatory,Course
WHERE ProgrammeMandatory.code = Course.code OR BranchMandatory.code = Course.code

SELECT Student.name as name,Course.name as coursename
FROM Student,HasTaken,Course
WHERE Student.persnumber = HasTaken.persnumber AND Course.code = HasTaken.code; */