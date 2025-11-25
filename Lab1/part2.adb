with Ada.Text_IO;
with Ada.Calendar;
with Ada.Numerics.Float_Random;
use Ada.Text_IO;
use Ada.Calendar;
use Ada.Numerics.Float_Random;

procedure Part2 is

	-- F1 executes every second
	F1_Interval : constant Duration := 1.0;
	-- F3 executes every other second 
   	F3_Pause : constant Duration := 0.5;  
  	-- Deadline for F3
  	F3_Deadline : constant Duration := 0.5;

 	Start_Time : Time    := Clock;   
   	Step_Count : Integer := 0;      
   	Current_Time     : Time;
   	Time_Since_Start : Duration;

   	-- if F3 is late
   	F3_Missed_Deadline : Boolean := False;

	RandGen : Generator;

   	--Watchdog task
   	task Watchdog is
      		entry Start;
      		entry Done;
   	end Watchdog;

   	task body Watchdog is
	begin
		loop
         		accept Start;

         		select
	     			-- F3 finished before 0.5s
            			accept Done;  
         		or
            			-- Deadline passes
            			delay F3_Deadline;
            			F3_Missed_Deadline := True;
            			Put_Line ("WARNING: F3 deadline missed!");
	    			accept Done;
         		end select;
      		end loop;
   	end Watchdog;

   	procedure F3 is
      		Delay_Time : Duration;
   	begin
      		-- Random time between 0.2s and 0.8s
      		Delay_Time := 0.2 + Duration(Random(RandGen)) * 0.6;

      		delay Delay_Time; 
      		Put_Line ("F3 finished, execution time: " & Duration'Image(Delay_Time));
   	end F3;

	begin
   		Reset (RandGen);
   		Put_Line ("Cyclic scheduler with watchdog started");

   	loop
      		declare
         		F1_Start : constant Time := Start_Time + Duration (Step_Count) *  F1_Interval;
      		begin
         		delay until F1_Start;

         		-- F1
         		Current_Time     := Clock;
         		Time_Since_Start := Current_Time - Start_Time;
         		Put_Line ("F1 executing, time is now:" & Duration'Image (Time_Since_Start));

         		-- F2
         		Current_Time     := Clock;
         		Time_Since_Start := Current_Time - Start_Time;
         		Put_Line ("F2 executing, time is now:" & Duration'Image (Time_Since_Start));
      		end;

      		-- F3
      		if Step_Count mod 2 = 0 then
         		declare
            			F3_Start : constant Time := Start_Time + Duration (Step_Count) * F1_Interval + F3_Pause;
         		begin
            			delay until F3_Start;

            			-- reset flag before starting F3
            			F3_Missed_Deadline := False;

            			-- start watchdog and run F3
            			Watchdog.Start;
            			F3;
            			Watchdog.Done;
			end;
    		end if;

	-- if deadline was missed
      	if F3_Missed_Deadline then
         	Put_Line ("Resynchronising after missed F3 deadline");

         	Start_Time := Clock;
         	Step_Count := 0;
      	else
         	-- normal case
         	Step_Count := Step_Count + 1;
      	end if;
 	end loop;
end Part2;
