CREATE TABLE Department (
	name VARCHAR(64),
	PRIMARY KEY name
);

CREATE TABLE Programme (
	name VARCHAR(64),
	PRIMARY KEY name,
	CONSTRAINT HostedBy FOREIGN KEY name REFERENCES Department(name)
);

CREATE TABLE Course (
	code				CHAR(6),
	name				VARCHAR(64),
	credits			INT,
	maxstudents INT,
	PRIMARY KEY code,
	CONSTRAINT GivenBy FOREIGN KEY name REFERENCES Department(name)
);

CREATE TABLE PreReq (
	course CHAR(6) REFERENCES Course(code),
	prereqcourse CHAR(6) REFERENCES Course(code)
);

CREATE TABLE CourseClass (
	classname VARCHAR(64)
	PRIMARY KEY classname
);

CREATE TABLE Student (
	persnumber 	CHAR(12),
	name				VARCHAR(64),
	PRIMARY KEY persnumber
);

CREATE TABLE Registered (
	persnumber CHAR(12),
	code       CHAR(6),
	FOREIGN KEY persnumber REFERENCES Student,
	FOREIGN KEY code REFERENCES Course,
);

CREATE TABLE HasTaken (
	persnumber CHAR(12),
	code       CHAR(6),
	grade			 CHAR(1),
	FOREIGN KEY persnumber REFERENCES Student,
	FOREIGN KEY code REFERENCES Course,
	CONSTRAINT ValidGrade CHECK (grade in ('U',3,4,5))
);