CREATE TABLE Department (
	name VARCHAR(64),
	PRIMARY KEY name
);

CREATE TABLE Course (
	code CHAR(6),
	name VARCHAR(64),
	credits INT,
	PRIMARY KEY code,
	CONSTRAINT GivenBy FOREIGN KEY name REFERENCES Department
);

