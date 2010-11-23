-- For all students, their names, and the programme and branch they are following.
CREATE VIEW DBStudents AS
	SELECT Student.persnumber as persnumber, Student.name as name,programme,branch 
	FROM Student,Branch
	WHERE programme = Branch.progname

-- For all students, all finished courses, along with their grades.
CREATE VIEW DBFinishedCourses AS 
	SELECT Student.persnumber as persnumber, Student.name as name, Course.name as coursename, grade
	FROM HasTaken,Student,Course
	WHERE Student.persnumber = HasTaken.persnumber
	AND Course.code = HasTaken.code;

-- All registered and waiting students for all courses, along with their waiting status ('registered' or 'waiting').
CREATE VIEW DBStudentStatus AS
	SELECT * FROM (SELECT Student.persnumber as persnumber, Student.name as name,Course.name as coursename,'registered' as status
		FROM Student,Registered,Course
		WHERE Student.persnumber = Registered.persnumber
			AND Course.code = Registered.code) UNION
	(SELECT Student.persnumber as persnumber, Student.name as name, Course.name as coursename,'waiting' as status
		FROM Student,WaitingList,Course
		WHERE Student.persnumber = WaitingList.persnumber
			AND Course.code = WaitingList.code);
			

-- For all students, the mandatory courses (branch and programme) they have not yet taken.
CREATE VIEW DBMandatoryCourses AS 
	((SELECT Student.persnumber as persnumber, Student.name as name, Course.name as coursename
		FROM Student,BranchMandatory,Course
		WHERE BranchMandatory.branch = Student.branch AND BranchMandatory.programme = Student.programme AND Course.code = BranchMandatory.code)
		UNION
		(SELECT Student.persnumber as persnumber, Student.name as name, Course.name as coursename
			FROM Student,ProgrammeMandatory,Course
			WHERE ProgrammeMandatory.programme = Student.programme AND Course.code = ProgrammeMandatory.code))
	MINUS
	(SELECT Student.persnumber as persnumber, Student.name as name,Course.name as coursename
		FROM Student,HasTaken,Course
		WHERE Student.persnumber = HasTaken.persnumber AND Course.code = HasTaken.code);

-- For all students, the recommended courses of their branch that they have not yet taken.
CREATE VIEW DBRecommendedCourses AS 
	(SELECT Student.persnumber as persnumber, Student.name as name, Course.name as coursename
		FROM Student,BranchRecommended,Course
		WHERE BranchRecommended.branch = Student.branch AND BranchRecommended.programme = Student.programme AND Course.code = BranchRecommended.code)
	MINUS
	(SELECT Student.persnumber as persnumber, Student.name as name, Course.name as coursename
		FROM Student,HasTaken,Course
		WHERE Student.persnumber = HasTaken.persnumber AND Course.code = HasTaken.code);


-- sub-part of last view: get sum of credits for a student
SELECT Student.persnumber, SUM(Course.credits)
	FROM Student LEFT OUTER JOIN HasTaken ON Student.persnumber = HasTaken.persnumber LEFT OUTER JOIN Course ON HasTaken.code = Course.code
	GROUP BY Student.persnumber);

-- the number of mandatory courses they have yet to read (branch or programme).
SELECT Student.persnumber, SUM(Course.credits)
	FROM Student LEFT OUTER JOIN DBMandatoryCourses ON Student.persnumber = DBMandatoryCourses.persnumber LEFT OUTER JOIN Course ON HasTaken.code = Course.code
	GROUP BY Student.persnumber);
	
-- the number of branch-specific (mandatory and recommended) credits they have taken.
	
/*
(SELECT Student.persnumber, SUM(Course.credits) as credits
FROM Student JOIN HasTaken ON Student.persnumber =  HasTaken.persnumber
JOIN Course ON Course.code = HasTaken.code
		WHERE HasTaken.grade = ANY (3,4,5) GROUP BY Student.persnumber)


SELECT Student.persnumber, SUM(Course.credits)
FROM Student LEFT OUTER JOIN HasTaken ON Student.persnumber =  HasTaken.persnumber LEFT OUTER JOIN Course ON HasTaken.code = Course.code GROUP BY Student.persnumber
*/

/*
SELECT Student.name as name,Course.name as coursename
FROM Student,BranchMandatory,ProgrammeMandatory,Course
WHERE ProgrammeMandatory.code = Course.code OR BranchMandatory.code = Course.code

SELECT Student.name as name,Course.name as coursename
FROM Student,HasTaken,Course
WHERE Student.persnumber = HasTaken.persnumber AND Course.code = HasTaken.code; */
