CREATE VIEW DBStudents AS
	SELECT name,programme,branch 
	FROM Student,Branch
	WHERE programme = Branch.progname

CREATE VIEW DBStudentStatus AS
	SELECT * FROM (SELECT Student.name as studentname,Course.name as coursename,'registered' as status
		FROM Student,Registered,Course
		WHERE Student.persnumber = Registered.persnumber
			AND Course.code = Registered.code) UNION
	(SELECT Student.name as studentname,Course.name as coursename,'waiting' as status
		FROM Student,WaitingList,Course
		WHERE Student.persnumber = WaitingList.persnumber
			AND Course.code = WaitingList.code);