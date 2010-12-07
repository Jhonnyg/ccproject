------------------------------------------------------------------------------
Databases (HT2010) - Assignment - Task 3 - Submission 3
2010-12-07

Sven Andersson, 19860708-4632
Jhonny Göransson, 19840611-8235
------------------------------------------------------------------------------
-- Updated 2010-12-07:
>Argh, I also forget to tell you to check, in "CourseRegistering",
>whether the student you try to insert has not already read the course.
Check!

>Also, you have to take care of the 'U's in "HasTaken".
Check!

>In "CourseUnregistration", one would expect do delete a student from the
>queue also; in your case, it turns the status from 'waiting' to 'registered' oO.
Check!

>Your task is almost acceptable, but the previous paragraph is a bullet
>of the assignment of the task 4 :P
Should pass now! :)

-- Updated 2010-12-06:
Added a check so that people will not be able to register for a course they
haven't taken all prerequisites for.

-- Original submission:
Oracle L/P: vtda357_014 / pxfpxf

Files:
	triggers.sql - Our two triggers.
	testdata.sql - Description and SQL for a test case.

------------------------------------------------------------------------------