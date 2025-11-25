with Ada.Text_IO;
with Ada.Calendar;
use Ada.Text_IO;
use Ada.Calendar;

procedure Part1 is

	-- F1 executes every second
	F1_Interval : constant Duration := 1.0;
	-- F3 executes every other second 
	F3_Pause : constant Duration := 0.5;  

	Start_Time : constant Time := Clock;
	Step_Count : Integer := 0;  
   	Current_Time     : Time;
   	Time_Since_Start : Duration;

begin
  	loop
      		declare
         		F1_Start : constant Time := Start_Time + Duration (Step_Count) *  F1_Interval;
      		begin
         		delay until F1_Start;

         		-- F1
         		Current_Time     := Clock;
         		Time_Since_Start := Current_Time - Start_Time;
         		Put_Line ("F1 executing, time is now:" & Duration'Image (Time_Since_Start));

         		-- F2 starts when F1 terminates
         		Current_Time     := Clock;
         		Time_Since_Start := Current_Time - Start_Time;
         		Put_Line ("F2 executing, time is now:" & Duration'Image (Time_Since_Start));
      		end;

      		-- F3 executes every other second, starting 0.5s after F1’s start
      		if Step_Count mod 2 = 0 then
         		declare
            			F3_Start : constant Time := Start_Time + Duration (Step_Count) * F1_Interval + F3_Pause;
         		begin
            			delay until F3_Start;

            			Current_Time     := Clock;
            			Time_Since_Start := Current_Time - Start_Time;
            			Put_Line ("F3 executing, time is now:" & Duration'Image (Time_Since_Start));
         		end;
      		end if;

      		-- Next step	
      		Step_Count := Step_Count + 1;
   	end loop;
end Part1;