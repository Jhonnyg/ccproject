CREATE VIEW DBStudents AS
	SELECT name,programme,branch 
	FROM Student,Branch
	WHERE programme = Branch.progname
