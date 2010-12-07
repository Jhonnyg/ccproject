-- when a student tries to register for a course that is full, that student is added to the waiting list for the course.
-- Be sure to check that the student may actually register for the course before adding to either list.
CREATE OR REPLACE TRIGGER CourseRegistering
	INSTEAD OF INSERT ON DBStudentStatus
	REFERENCING NEW AS newreg
	FOR EACH ROW
	DECLARE
		limited INT;
		currentNum INT;
		prereqleft INT;
		allreadyTaken INT;
	BEGIN
		SELECT COUNT(*) INTO allreadyTaken FROM
			(SELECT code FROM HasTaken WHERE persnumber = :newreg.persnumber AND HasTaken.code = :newreg.coursecode);
		
		-- check so that the student hasn't taken the course allready
		IF allreadyTaken = 0 THEN		
			SELECT COUNT(*) INTO prereqleft FROM
				((SELECT prereqcourse as course FROM PreReq WHERE PreReq.course = :newreg.coursecode)
				 MINUS
				 (SELECT code as course FROM HasTaken WHERE persnumber = :newreg.persnumber AND HasTaken.grade = ANY ('3','4','5')));
			
			IF prereqleft = 0 THEN
				-- no pre req courses left
				SELECT maxstudents INTO limited FROM Course WHERE code = :newreg.coursecode;
				IF limited IS NOT NULL THEN
					SELECT COUNT (*) INTO currentNum FROM Registered WHERE code = :newreg.coursecode;
					IF currentNum < limited THEN -- still some places left on the course
						INSERT INTO Registered VALUES(:newreg.persnumber, :newreg.coursecode);
					ELSE -- otherwise; put on waiting list
						INSERT INTO WaitingList VALUES(:newreg.persnumber, :newreg.coursecode, sysdate);
					END IF;
				ELSE -- not a limited course, just insert!
					INSERT INTO Registered VALUES(:newreg.persnumber, :newreg.coursecode);
				END IF;
			END IF;
		
		END IF;
	END;


-- when a student unregisters from a course (and was previously registered and not only in the waiting list),
-- the first student (if any) in the waiting list should be registered for the course instead. Hint: write the
-- triggers on the view you created for registrations instead of on the tables themselves (third bullet under subtask 3 above).
CREATE OR REPLACE TRIGGER CourseUnregistration
INSTEAD OF DELETE ON DBStudentStatus
REFERENCING OLD AS oldreg
FOR EACH ROW
DECLARE
firstStuInQueue CHAR(12);
waitingNum INT;
inWaitingList INT;
BEGIN
	SELECT COUNT(*) INTO inWaitingList FROM
		(SELECT code FROM WaitingList WHERE persnumber = :oldreg.persnumber AND WaitingList.code = :oldreg.coursecode);
	
	IF inWaitingList = 0 THEN
		DELETE FROM Registered WHERE persnumber = :oldreg.persnumber AND code = :oldreg.coursecode;

		SELECT COUNT(persnumber) INTO waitingNum FROM WaitingList WHERE code = :oldreg.coursecode;
		IF waitingNum > 0 THEN --there are waiting students
			--SELECT persnumber INTO firstStuInQueue FROM WaitingList WHERE code = :oldreg.coursecode ORDER BY registertime DESC ROWNUM < 2; --first priority
			SELECT persnumber INTO firstStuInQueue
				FROM (SELECT persnumber FROM WaitingList WHERE code = :oldreg.coursecode ORDER BY registertime ASC)
			  WHERE ROWNUM = 1;
			INSERT INTO Registered VALUES(firstStuInQueue, :oldreg.coursecode); --first student in the queue
			DELETE FROM WaitingList WHERE persnumber = firstStuInQueue AND code = :oldreg.coursecode; --first student in the queue
			--UPDATE FROM WaitingList... --update the priority if necessary
		END IF;
	ELSE
		-- remove student from waiting list instead
		DELETE FROM WaitingList WHERE persnumber = :oldreg.persnumber AND code = :oldreg.coursecode;
	END IF;
END;






