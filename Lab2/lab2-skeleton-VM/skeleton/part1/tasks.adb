with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Real_Time;  use Ada.Real_Time;
with System;

with Webots_API;   use Webots_API;

package body Tasks is
  -------------
  --  Tasks  --
  -------------   
  task HelloworldTask is
    -- define its priority higher than the main procedure --
    -- main procedure priority is declared at main.adb:9
  end HelloworldTask;

  task body HelloworldTask is
    Next_Time : Time := Time_Zero;
    Light     : Integer;
    Dist      : Integer;

  begin      
    -- task body starts here ---
    -- Make the robot move (pick any value in [-999, 999])
    set_motor_speed (LeftMotor,  400);
    set_motor_speed (RightMotor, 400);

    loop
      -- read sensors and print (have a look at webots_api.ads) ----
      -- Read one ground-facing light sensor and print it
      Light := read_light_sensor (LS2);
      Put_Line ("LS2 = " & Integer'Image(Light));
      
      -- Read Distance sensor and print it
      Dist := read_distance_sensor;
      Put_Line ("Dist = " & Integer'Image(Dist));

      Next_Time := Next_Time + Period_Display;
      delay until Next_Time;

      exit when simulation_stopped;
    end loop;
  end HelloworldTask;

  -- Background procedure required for package
  procedure Background is begin
    while not simulation_stopped loop
      delay 0.25;
    end loop;
  end Background;

end Tasks;
