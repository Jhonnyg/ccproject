-- For all students, their names, and the programme and branch they are following.
CREATE VIEW DBStudents AS
	SELECT Student.persnumber as persnumber, Student.name as name,programme,branch 
	FROM Student,Branch
	WHERE programme = Branch.progname

-- For all students, all finished courses, along with their grades.
CREATE VIEW DBFinishedCourses AS 
	SELECT Student.persnumber as persnumber, Student.name as name, Course.code as coursecode, grade
	FROM HasTaken,Student,Course
	WHERE Student.persnumber = HasTaken.persnumber
	AND Course.code = HasTaken.code AND HasTaken.grade = ANY ('3','4','5');

-- All registered and waiting students for all courses, along with their waiting status ('registered' or 'waiting').
CREATE VIEW DBStudentStatus AS
	SELECT * FROM (SELECT Registered.persnumber as persnumber,Course.code as coursecode,'registered' as status
		FROM Registered,Course
		WHERE Course.code = Registered.code) UNION
	(SELECT WaitingList.persnumber as persnumber, Course.code as coursecode,'waiting' as status
		FROM WaitingList,Course
		WHERE Course.code = WaitingList.code);
			

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
		WHERE Student.persnumber = HasTaken.persnumber AND Course.code = HasTaken.code AND HasTaken.grade = ANY ('3','4','5'));


-- For all students, the recommended courses of their branch that they have not yet taken.
CREATE VIEW DBRecommendedCourses AS 
	(SELECT Student.persnumber as persnumber, Student.name as name, Course.name as coursename
		FROM Student,BranchRecommended,Course
		WHERE BranchRecommended.branch = Student.branch AND BranchRecommended.programme = Student.programme AND Course.code = BranchRecommended.code)
	MINUS
	(SELECT Student.persnumber as persnumber, Student.name as name, Course.name as coursename
		FROM Student,HasTaken,Course
		WHERE Student.persnumber = HasTaken.persnumber AND Course.code = HasTaken.code AND HasTaken.grade = ANY ('3','4','5'));


CREATE VIEW DBStudentSummary AS
-- sub-part of last view: get sum of credits for a student
SELECT pnumber as persnumber, totalcredits, branchspec_credits, mandatoryleft, mathcredits, researchcredits, numseminar FROM

(SELECT Student.persnumber as pnumber, SUM(Course.credits) as totalcredits
	FROM Student LEFT OUTER JOIN HasTaken ON Student.persnumber = HasTaken.persnumber LEFT OUTER JOIN Course ON HasTaken.code = Course.code AND HasTaken.grade = ANY ('3','4','5')
	GROUP BY Student.persnumber) totalcreditstable

LEFT OUTER JOIN

-- the number of branch-specific (mandatory and recommended) credits they have taken.
(SELECT Student.persnumber as pnumber, SUM(Course.credits) as branchspec_credits
	FROM Student LEFT OUTER JOIN HasTaken ON Student.persnumber = HasTaken.persnumber
	LEFT OUTER JOIN ((SELECT BranchMandatory.code as ccode, BranchMandatory.branch as bbranch FROM BranchMandatory)
	                  UNION
	                 (SELECT BranchRecommended.code as ccode, BranchRecommended.branch as bbranch FROM BranchRecommended)) ON ccode = HasTaken.code AND bbranch = Student.branch AND HasTaken.grade = ANY ('3','4','5')
	LEFT OUTER JOIN Course ON ccode = Course.code
	GROUP BY Student.persnumber) branchspectable
ON totalcreditstable.pnumber = branchspectable.pnumber
LEFT OUTER JOIN

-- the number of mandatory courses they have yet to read (branch or programme).
(SELECT Student.persnumber as pnumber, COUNT(DBMandatoryCourses.coursename) as mandatoryleft
	FROM Student LEFT OUTER JOIN DBMandatoryCourses ON Student.persnumber = DBMandatoryCourses.persnumber
	GROUP BY Student.persnumber) mandatorylefttable
ON totalcreditstable.pnumber = mandatorylefttable.pnumber
LEFT OUTER JOIN
	
-- the number of credits they have taken in courses that are classified as math courses.
(SELECT Student.persnumber as pnumber, SUM(credits) as mathcredits
	FROM Student LEFT OUTER JOIN HasTaken ON Student.persnumber = HasTaken.persnumber LEFT OUTER JOIN (SELECT Course.code as ccode,Course.credits as credits FROM Course,CourseClass WHERE Course.code = CourseClass.code AND CourseClass.classname = 'mathematical') ON HasTaken.code = ccode AND HasTaken.grade = ANY ('3','4','5')
	GROUP BY Student.persnumber) mathcreditstable
ON totalcreditstable.pnumber = mathcreditstable.pnumber
LEFT OUTER JOIN
	
-- the number of credits they have taken in courses that are classified as research courses.
(SELECT Student.persnumber as pnumber, SUM(credits) as researchcredits
	FROM Student LEFT OUTER JOIN HasTaken ON Student.persnumber = HasTaken.persnumber LEFT OUTER JOIN (SELECT Course.code as ccode,Course.credits as credits FROM Course,CourseClass WHERE Course.code = CourseClass.code AND CourseClass.classname = 'research') ON HasTaken.code = ccode AND HasTaken.grade = ANY ('3','4','5')
	GROUP BY Student.persnumber) researchcreditstable
ON totalcreditstable.pnumber = researchcreditstable.pnumber
LEFT OUTER JOIN
	
-- the number of seminar courses they have read.
(SELECT Student.persnumber as pnumber, COUNT(ccode) as numseminar
	FROM Student LEFT OUTER JOIN HasTaken ON Student.persnumber = HasTaken.persnumber LEFT OUTER JOIN (SELECT Course.code as ccode FROM Course,CourseClass WHERE Course.code = CourseClass.code AND CourseClass.classname = 'seminar') ON HasTaken.code = ccode AND HasTaken.grade = ANY ('3','4','5')
	GROUP BY Student.persnumber) numseminartable
ON totalcreditstable.pnumber = numseminartable.pnumber;

-- final composit of studentsummary and a cangraduate column
CREATE VIEW DBCanGraduate AS
	SELECT persnumber, totalcredits, branchspec_credits, mandatoryleft, mathcredits, researchcredits, numseminar, CASE
		WHEN DBStudentSummary.mandatoryleft = 0 AND DBStudentSummary.branchspec_credits >= 10 AND DBStudentSummary.mathcredits >= 20 AND DBStudentSummary.researchcredits >= 10 AND DBStudentSummary.numseminar >= 1
			THEN 'YES'
			ELSE 'NO'
		END as cangraduate
	FROM DBStudentSummary;
	
