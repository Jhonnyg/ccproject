-- Departments	
INSERT INTO Department
	VALUES ('Computing Science');
	
INSERT INTO Department
	VALUES ('Signals and Systems');
	
-- Programmes
INSERT INTO Programme
	VALUES ('Computer Science and Engineering', 'Computing Science');

-- Branches	
INSERT INTO Branch
	VALUES ('Computer Languages','Computer Science and Engineering');
	
-- Courses
INSERT INTO Course 
	(code,name,credits,depname)
	VALUES ('TDA357','Databases',10, 'Computing Science');
	
	
	
	
INSERT INTO Student
	VALUES (8406118235,'Jhonny Göransson', 'Computer Science and Engineering', 'Computer Languages');

INSERT INTO Student
	VALUES (8601011337,'Sven Andersson', 'Computer Science and Engineering', 'Computer Languages');

INSERT INTO Student
	VALUES (8611283550,'Ingemar Åhdal', 'Computer Science and Engineering', 'Computer Languages');