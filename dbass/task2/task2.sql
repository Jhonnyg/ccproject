CREATE TABLE Department (
	name VARCHAR(64),
	PRIMARY KEY (name)
);

CREATE TABLE Programme (
	name 	VARCHAR(64),
	depname VARCHAR(64),
	PRIMARY KEY (name),
	CONSTRAINT HostedBy FOREIGN KEY (depname) REFERENCES Department(name)
);

CREATE TABLE Branch(
	name	VARCHAR(64),
	progname	VARCHAR(64),
	PRIMARY KEY (name, progname),
	CONSTRAINT BranchOf FOREIGN KEY (progname) REFERENCES Programme(name)
);

CREATE TABLE Course (
	code		CHAR(6),
	name		VARCHAR(64),
	depname		VARCHAR(64),
	credits		INT,
	maxstudents INT DEFAULT NULL,
	PRIMARY KEY (code),
	CONSTRAINT GivenBy FOREIGN KEY (depname) REFERENCES Department(name)
);

CREATE TABLE PreReq (
	course CHAR(6) REFERENCES Course(code),
	prereqcourse CHAR(6) REFERENCES Course(code)
);

CREATE TABLE CourseClass (
	classname VARCHAR(64),
	PRIMARY KEY (classname)
);

CREATE TABLE Student (
	persnumber 	CHAR(12),
	name		VARCHAR(64),
	branch	VARCHAR(64),
	programme	VARCHAR(64),
	PRIMARY KEY (persnumber),
	CONSTRAINT BelongsTo FOREIGN KEY (branch, programme) REFERENCES Branch(name, progname)
);

CREATE TABLE Registered (
	persnumber CHAR(12),
	code       CHAR(6),
	FOREIGN KEY (persnumber) REFERENCES Student,
	FOREIGN KEY (code) REFERENCES Course
);

CREATE TABLE WaitingList (
	persnumber CHAR(12),
	code       CHAR(6),
	registertime	INT,
	FOREIGN KEY (persnumber) REFERENCES Student,
	FOREIGN KEY (code) REFERENCES Course(code)
);

CREATE TABLE HasTaken (
	persnumber CHAR(12),
	code       CHAR(6),
	grade	   CHAR(1),
	FOREIGN KEY (persnumber) REFERENCES Student,
	FOREIGN KEY (code) REFERENCES Course(code),
	CONSTRAINT ValidGrade CHECK (grade in ('U',3,4,5))
);

CREATE TABLE ProgrammeMandatory (
	programme VARCHAR(64),
	code	CHAR(6),
	FOREIGN KEY (programme) REFERENCES Programme(name),
	FOREIGN KEY (code) REFERENCES Course(code)
);

CREATE TABLE BranchMandatory (
	programme VARCHAR(64),
	branch	VARCHAR(64),
	code	CHAR(6),
	FOREIGN KEY (branch, programme) REFERENCES Branch(name, progname),
	FOREIGN KEY (code) REFERENCES Course(code)
);

CREATE TABLE BranchRecommended (
	programme VARCHAR(64),
	branch	VARCHAR(64),
	code	CHAR(6),
	FOREIGN KEY (branch, programme) REFERENCES Branch(name, progname),
	FOREIGN KEY (code) REFERENCES Course(code)
);



