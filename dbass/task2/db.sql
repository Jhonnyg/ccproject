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
	VALUES ('Computer Languages', 'Computer Science and Engineering');
	
-- Courses
INSERT INTO Course 
	(code,name,credits,depname)
	VALUES ('TDA357','Databases',10, 'Computing Science');
	
	
-- Students
INSERT INTO Student
	VALUES ('8406118235', 'Jhonny Göransson', 'Computer Languages', 'Computer Science and Engineering');

INSERT INTO Student
	VALUES ('8607084632','Sven Andersson', 'Computer Languages', 'Computer Science and Engineering');

INSERT INTO Student
	VALUES ('8611283550','Ingemar Åhdal', 'Computer Languages', 'Computer Science and Engineering');
	

-- Courses registered
INSERT INTO Registered VALUES ('8607084632', 'TDA357')

-- Courses waiting lists
INSERT INTO WaitingList VALUES ('8607084632', 'TDA357', 123456)

