-- first add two students to a course (with maximum student count of 2)
INSERT INTO DBStudentStatus VALUES('8607084632', 'TDA357', 'registered');
INSERT INTO DBStudentStatus VALUES('8406118235', 'TDA357', 'registered');

-- then add two more student to the same course, they should appear in the waiting list
INSERT INTO DBStudentStatus VALUES('8611283550', 'TDA357', 'registered');
INSERT INTO DBStudentStatus VALUES('8701983522', 'TDA357', 'registered');

-- now lets remove one of the two first students, thus making room for one of the waiting students
DELETE FROM DBStudentStatus WHERE persnumber = '8607084632' AND coursecode = 'TDA357';

-- Now students '8406118235' and '8611283550' should be registered on the course,
-- and student '8701983522' should still be on the waiting list (since he tried to register last).
SELECT * FROM DBStudentStatus;

-- Finally try to add a student that hasn't taken the prerequisite course (which is 'TDA357) for 'TDA335'
INSERT INTO DBStudentStatus VALUES('8607084632', 'TDA335', 'registered');

-- The output should be the same as before, i.e. no row should be added.
SELECT * FROM DBStudentStatus;

