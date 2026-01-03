with Ada.Real_Time;       use Ada.Real_Time;
-- Add required sensor and actuator package --

package Tasks is
  procedure Background;
private

  --  Define periods and times  --
  Period_Display : Time_Span := Milliseconds(200); 
  Time_Zero      : Time := Clock;
      
  --  Other specifications  --
  Period_EventDispatcher : constant Time_Span := Milliseconds (10);   -- ~10 ms polling
  
  -- Periodic schedule (tune if needed)
  Period_Motor   : constant Time_Span := Milliseconds (10);
  Period_Line    : constant Time_Span := Milliseconds (10);
  Period_Dist    : constant Time_Span := Milliseconds (20);
  
  -- --- Line detection threshold (tune if needed) ---
  -- Typically white is higher and black is lower. Adjust after quick test in Part 1.
  Black_Threshold : constant Integer := 600;

  -- --- Motor speeds in [-999, 999] ---
  Speed_Forward   : constant Integer := 400;
  Speed_Backward  : constant Integer := -400;
  Speed_Turn      : constant Integer := 200;
  
  Base_Speed  : constant Integer := 400;  -- forward speed on clear track
  Max_Turn    : constant Integer := 100;  -- clamp for steering term
  
  -- Obstacle distance (units are Webots raw sensor units)
  -- If it stops too far away -> lower Stop_Dist.
  -- If it crashes into box -> raise Stop_Dist.
  Slow_Dist : constant Integer := 100;
  Stop_Dist : constant Integer := 800;

end Tasks;
