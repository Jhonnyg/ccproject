-- Departments	
INSERT INTO Department
	VALUES ('Computing Science');
	
INSERT INTO Department
	VALUES ('Signals and Systems');
	
-- Programmes
INSERT INTO Programme
	VALUES ('Computer Science and Engineering', 'Computing Science');

INSERT INTO Programme
	VALUES ('Automation Control, Automation And Mechatronics', 'Signals and Systems');

-- Branches	
INSERT INTO Branch
	VALUES ('Computer Languages', 'Computer Science and Engineering');
	
INSERT INTO Branch
	VALUES ('Mechatronics', 'Automation Control, Automation And Mechatronics');
	
-- Courses
INSERT INTO Course 
	(code,name,credits,depname)
	VALUES ('TDA357','Databases',10, 'Computing Science', 2);
	
INSERT INTO Course
	(code,name,credits,depname)
	VALUES ('TDA335','Programming Project, Major',10,'Computing Science');
	
INSERT INTO Course
	(code,name,credits,depname)
	VALUES ('TDA112','How to make robot',10,'Signals and Systems');
	
INSERT INTO Course
	(code,name,credits,depname)
	VALUES ('DAT026','Mathematical modelling',10,'Computing Science');

INSERT INTO Course
	(code,name,credits,depname)
	VALUES ('DAT235','Research-oriented special course',10,'Computing Science');

INSERT INTO Course
	(code,name,credits,depname)
	VALUES ('TDA251','Algorithms, advanced course',10,'Computing Science');

-- Classified courses
INSERT INTO CourseClass
	VALUES ('mathematical', 'DAT026');
	
INSERT INTO CourseClass
	VALUES ('research', 'DAT235');
	
INSERT INTO CourseClass
	VALUES ('seminar', 'TDA335');
	
INSERT INTO CourseClass
	VALUES ('mathematical', 'TDA335');
	
INSERT INTO CourseClass
	VALUES ('mathematical', 'TDA357');

-- Courses that are mandatory under a specific branch
INSERT INTO ProgrammeMandatory
	VALUES ('Automation Control, Automation And Mechatronics','TDA357');

-- Courses that are mandatory under a specific branch
INSERT INTO BranchMandatory
	VALUES ('Computer Science and Engineering','Computer Languages','TDA335');
	
INSERT INTO BranchMandatory
	VALUES ('Computer Science and Engineering','Computer Languages','TDA357');
	
INSERT INTO BranchMandatory
	VALUES ('Automation Control, Automation And Mechatronics','Mechatronics','TDA112');
	
-- Courses that are recommended under a specific branch
INSERT INTO BranchRecommended
	VALUES ('Computer Science and Engineering','Computer Languages','TDA251');	
	
-- Students
INSERT INTO Student
	VALUES ('8406118235', 'Jhonny Göransson', 'Computer Languages', 'Computer Science and Engineering');

INSERT INTO Student
	VALUES ('8607084632','Sven Andersson', 'Computer Languages', 'Computer Science and Engineering');

INSERT INTO Student
	VALUES ('8611283550','Ingemar Åhdal', 'Mechatronics', 'Automation Control, Automation And Mechatronics');
	
INSERT INTO Student
	VALUES ('8701983522','Filip Lundborg', 'Mechatronics', 'Automation Control, Automation And Mechatronics');
	
-- Courses registered
INSERT INTO Registered VALUES ('8607084632', 'TDA357');

-- Courses waiting lists
INSERT INTO WaitingList VALUES ('8607084632', 'TDA357', 123456);

-- Courses history and grade
INSERT INTO HasTaken VALUES ('8406118235','TDA357',5);
INSERT INTO HasTaken VALUES ('8406118235','TDA335',3);
INSERT INTO HasTaken VALUES ('8406118235','TDA251',4);
INSERT INTO HasTaken VALUES ('8406118235','DAT235',3);
INSERT INTO HasTaken VALUES ('8406118235','TDA112',5);

INSERT INTO HasTaken VALUES ('8607084632','TDA335','U');
INSERT INTO HasTaken VALUES ('8611283550','TDA112','3');
INSERT INTO HasTaken VALUES ('8701983522','DAT235',4);
INSERT INTO HasTaken VALUES ('8701983522','TDA357',5);