with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO;   use Ada.Text_IO;
with System;

with Webots_API;    use Webots_API;

package body Tasks is

  -----------------------------
  -- Shared command (protected, shared data but mutual exclusion)
  -----------------------------
  protected SharedCmd is
    procedure Set_Line (Left, Right : Integer);
    procedure Set_Distance_Factor (Factor : Integer); -- 1000 = full speed, 0 = stop
    procedure Get (Left, Right : out Integer; Factor : out Integer);
  private
    pragma Priority (System.Priority'First + 2);
    Line_Left  : Integer := 0;
    Line_Right : Integer := 0;
    Dist_F     : Integer := 1000;
  end SharedCmd;

  protected body SharedCmd is
    procedure Set_Line (Left, Right : Integer) is
    begin
      Line_Left  := Left;
      Line_Right := Right;
    end Set_Line;

    procedure Set_Distance_Factor (Factor : Integer) is
    begin
      if Factor < 0 then
        Dist_F := 0;
      elsif Factor > 1000 then
        Dist_F := 1000;
      else
        Dist_F := Factor;
      end if;
    end Set_Distance_Factor;

    procedure Get (Left, Right : out Integer; Factor : out Integer) is
    begin
      Left := Line_Left;
      Right := Line_Right;
      Factor := Dist_F;
    end Get;
  end SharedCmd;

  -----------------------------
  -- Helper functions
  -----------------------------
  function Clamp (V, Lo, Hi : Integer) return Integer is
  begin
    if V < Lo then
      return Lo;
    elsif V > Hi then
      return Hi;
    else
      return V;
    end if;
  end Clamp;


  function ClampF (V, Lo, Hi : Float) return Float is
  begin
    if V < Lo then
      return Lo;
    elsif V > Hi then
      return Hi;
    else
      return V;
    end if;
  end ClampF;
  
  -----------------------------
  -- MotorControlTask
  -----------------------------
  task MotorControlTask is
    pragma Priority (System.Priority'First + 2);
  end MotorControlTask;

  task body MotorControlTask is
    Next_Time : Time := Clock;
    L, R, F : Integer;
    OutL, OutR : Integer;
  begin
    -- start stopped
    set_motor_speed (LeftMotor,  0);
    set_motor_speed (RightMotor, 0);

    loop
      SharedCmd.Get (L, R, F);

      -- Apply distance factor: scaled = value * F / 1000
      OutL := (L * F) / 1000;
      OutR := (R * F) / 1000;

      OutL := Clamp (OutL, -999, 999);
      OutR := Clamp (OutR, -999, 999);

      set_motor_speed (LeftMotor,  OutL);
      set_motor_speed (RightMotor, OutR);

      exit when simulation_stopped;

      Next_Time := Next_Time + Period_Motor;
      delay until Next_Time;
    end loop;
  end MotorControlTask;

  -----------------------------
  -- LineFollowingTask
  -- Reads LS1/LS2/LS3, computes desired motor speeds,
  -- writes to SharedCmd (does NOT set motors directly).
  -----------------------------
  task LineFollowingTask is
    pragma Priority (System.Priority'First + 1);
  end LineFollowingTask;

  task body LineFollowingTask is
    Next_Time : Time := Clock;
    L1, L2, L3 : Integer;

    -- Implement a PI Controller
    E    : Float;
    U    : Float;
    -- PI controller state (history)
    I    : Float := 0.0;  -- Integral
    -- Sampling period in seconds (match Period_Line!)
    Ts   : constant Float := 0.01;  -- 10ms
    -- PI gains (tune these)
    Kp   : constant Float := 0.30;
    Ki   : constant Float := 0.10;
    
    -- Anti-windup limits
    Umax  : constant Float := Float(Max_Turn);
    Imax  : constant Float := (if Ki > 0.0001 then Umax / Ki else 0.0);
    
    -- Deadband to filter out noise
    Deadband : constant Float := 100.0; 

    Turn : Integer;
    DesiredL, DesiredR : Integer;

    -- helper: black detection
    Left_Black, Center_Black, Right_Black : Boolean;
  begin
    loop
      L1 := read_light_sensor (LS1);
      L2 := read_light_sensor (LS2);
      L3 := read_light_sensor (LS3);

      Left_Black   := (L1 < Black_Threshold);
      Center_Black := (L2 < Black_Threshold);
      Right_Black  := (L3 < Black_Threshold);
      
      -- error: left minus right 
      E := Float(L3 - L1);
      E := (if abs(E)> Deadband then E else 0.0); -- filter out noise
      
      -- Integrate error (with clamping to avoid windup)
      I := I + E * Ts;
      I := ClampF(I, -IMax, IMax);
      
      -- PI controller output
      U := Kp * E + Ki * I;      

      -- Clamp the turn command to what motors can reasonably do
      U := ClampF(U, -Umax, Umax);      

      -- Simple policy:
      -- 1) If center on black -> go straight (small steering correction based on L1-L3)
      -- 2) If left on black -> turn left
      -- 3) If right on black -> turn right
      -- 4) If none sees black -> keep searching using last-sign of error (fallback)
      if Center_Black then
        Turn := Clamp (Integer(U), -Max_Turn, Max_Turn);
        DesiredL := Base_Speed - Turn;
        DesiredR := Base_Speed + Turn;

      elsif Left_Black then
        DesiredL := Base_Speed - Max_Turn;
        DesiredR := Base_Speed + Max_Turn;

      elsif Right_Black then
        DesiredL := Base_Speed + Max_Turn;
        DesiredR := Base_Speed - Max_Turn;

      else
        -- lost line: rotate gently to reacquire (based on sign of Err)
        if I >= 0.0 then
          DesiredL := 150;
          DesiredR := -150;
        else
          DesiredL := -150;
          DesiredR := 150;
        end if;
      end if;

      DesiredL := Clamp (DesiredL, -999, 999);
      DesiredR := Clamp (DesiredR, -999, 999);

      SharedCmd.Set_Line (DesiredL, DesiredR);

      exit when simulation_stopped;

      Next_Time := Next_Time + Period_Line;
      delay until Next_Time;
    end loop;
  end LineFollowingTask;

  -----------------------------
  -- DistanceTask
  -- Reads front distance sensor and sets a speed factor:
  -- far -> 1000, near -> slow, too near -> stop.
  -----------------------------
  task DistanceTask is
    pragma Priority (System.Priority'First + 1);
  end DistanceTask;

  task body DistanceTask is
    Next_Time : Time := Clock;
    D : Integer;
    F : Integer;
  begin
    loop
      D := read_distance_sensor;

      -- Map distance reading to factor 0..1000 (bigger reading => closer obstacle)
      if D >= Stop_Dist then
        F := 0; -- stop
      elsif D <= Slow_Dist then
        F := 1000; -- full speed
      else
        -- linear ramp between Slow_Dist .. Stop_Dist
        -- D=Slow_Dist -> 1000, D=Stop_Dist -> 0
        F := (Stop_Dist - D) * 1000 / (Stop_Dist - Slow_Dist);
      end if;

      SharedCmd.Set_Distance_Factor (F);

      exit when simulation_stopped;

      Next_Time := Next_Time + Period_Dist;
      delay until Next_Time;
    end loop;
  end DistanceTask;

  -----------------------------
  -- DisplayTask 
  -----------------------------
  task DisplayTask is
    pragma Priority (System.Priority'First + 1);
  end DisplayTask;

  task body DisplayTask is
    Next_Time : Time := Clock;
    L1, L2, L3, D : Integer;
    L, R, F : Integer;
  begin
    loop
      L1 := read_light_sensor (LS1);
      L2 := read_light_sensor (LS2);
      L3 := read_light_sensor (LS3);
      D  := read_distance_sensor;

      SharedCmd.Get (L, R, F);

      Put_Line
        (" LS1=" & Integer'Image(L1) &
         " LS2=" & Integer'Image(L2) &
         " LS3=" & Integer'Image(L3) &
         " D="   & Integer'Image(D) &
         " CmdL=" & Integer'Image(L) &
         " CmdR=" & Integer'Image(R) &
         " F=" & Integer'Image(F));

      exit when simulation_stopped;

      Next_Time := Next_Time + Period_Display;
      delay until Next_Time;
    end loop;
  end DisplayTask;

  --------------------------
  -- Background procedure --
  --------------------------
  procedure Background is
  begin
    -- Do nothing; tasks run on their own.
    while not simulation_stopped loop
      delay 0.25;
    end loop;
  end Background;

end Tasks;
