with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO;   use Ada.Text_IO;
with System;

with Webots_API;    use Webots_API;

package body Tasks is

  --------------------------
  -- Event type and object --
  --------------------------
  type EventID is
    (UpPressed, UpReleased,
     DownPressed, DownReleased,
     LeftPressed, LeftReleased,
     RightPressed, RightReleased,
     LineBlack, LineWhite);

  protected Event is
    entry Wait (Id : out EventID);
    procedure Signal (Id : in EventID);
  private
   -- assign priority that is higher than the
   -- user tasks ' priorities
    pragma Priority (System.Priority'First + 2);
    Current_Id : EventID := UpReleased;
    Signalled  : Boolean := False;
  end Event;

  protected body Event is
    entry Wait (Id : out EventID) when Signalled is
    begin
      Id := Current_Id;
      Signalled := False;
    end Wait;

    procedure Signal (Id : in EventID) is
    begin
      Current_Id := Id;
      Signalled := True;
    end Signal;
  end Event;

  -------------------------
  -- Event dispatcher task
  -------------------------
  task EventDispatcherTask is
    pragma Priority (System.Priority'First + 1);
  end EventDispatcherTask;

  task body EventDispatcherTask is
    Next_Time : Time := Clock;

    -- previous key states
    Prev_Up, Prev_Down, Prev_Left, Prev_Right : Boolean := False;
    Cur_Up,  Cur_Down,  Cur_Left,  Cur_Right  : Boolean;

    -- previous line state
    Prev_Black : Boolean := False;
    Light      : Integer;
    Cur_Black  : Boolean;
  begin
    loop
      -- Key polling (edge detection)
      Cur_Up    := button_pressed (UpButton);
      Cur_Down  := button_pressed (DownButton);
      Cur_Left  := button_pressed (LeftButton);
      Cur_Right := button_pressed (RightButton);

      if Cur_Up and not Prev_Up then
        Event.Signal (UpPressed);
      elsif (not Cur_Up) and Prev_Up then
        Event.Signal (UpReleased);
      end if;

      if Cur_Down and not Prev_Down then
        Event.Signal (DownPressed);
      elsif (not Cur_Down) and Prev_Down then
        Event.Signal (DownReleased);
      end if;

      if Cur_Left and not Prev_Left then
        Event.Signal (LeftPressed);
      elsif (not Cur_Left) and Prev_Left then
        Event.Signal (LeftReleased);
      end if;

      if Cur_Right and not Prev_Right then
        Event.Signal (RightPressed);
      elsif (not Cur_Right) and Prev_Right then
        Event.Signal (RightReleased);
      end if;

      Prev_Up := Cur_Up;
      Prev_Down := Cur_Down;
      Prev_Left := Cur_Left;
      Prev_Right := Cur_Right;

      -- Line detection using LS2 (edge detection)
      Light := read_light_sensor (LS2);
      Cur_Black := (Light < Black_Threshold);

      if Cur_Black and not Prev_Black then
        Event.Signal (LineBlack);
      elsif (not Cur_Black) and Prev_Black then
        Event.Signal (LineWhite);
      end if;

      Prev_Black := Cur_Black;

      -- pace
      Next_Time := Next_Time + Period_EventDispatcher;
      delay until Next_Time;

      exit when simulation_stopped;
    end loop;
  end EventDispatcherTask;

  -------------------------
  -- Motor control task
  -------------------------
  task MotorControlTask is
    pragma Priority (System.Priority'First + 2);
  end MotorControlTask;

  task body MotorControlTask is
    E : EventID;

    -- internal state derived from events
    Up_Held, Down_Held, Left_Held, Right_Held : Boolean := False;
    On_Black : Boolean := False;

    procedure Stop is
    begin
      set_motor_speed (LeftMotor,  0);
      set_motor_speed (RightMotor, 0);
    end Stop;

    procedure Apply_Motion is
      Num : Natural := 0;
    begin
      -- black line => immediate stop
      if On_Black then
        Stop;
        return;
      end if;

      if Up_Held then Num := Num + 1; end if;
      if Down_Held then Num := Num + 1; end if;
      if Left_Held then Num := Num + 1; end if;
      if Right_Held then Num := Num + 1; end if;

      -- simplest rule: exactly one direction allowed, otherwise stop
      if Num /= 1 then
        Stop;
        return;
      end if;

      if Up_Held then
        set_motor_speed (LeftMotor,  Speed_Forward);
        set_motor_speed (RightMotor, Speed_Forward);
      elsif Down_Held then
        set_motor_speed (LeftMotor,  Speed_Backward);
        set_motor_speed (RightMotor, Speed_Backward);
      elsif Left_Held then
        set_motor_speed (LeftMotor,  -Speed_Turn);
        set_motor_speed (RightMotor, Speed_Turn);
      elsif Right_Held then
        set_motor_speed (LeftMotor,  Speed_Turn);
        set_motor_speed (RightMotor, -Speed_Turn);
      else
        Stop;
      end if;
    end Apply_Motion;

  begin
    Stop;

    loop
      Event.Wait (E);
      Put_Line("Event = " & EventID'Image(E));

      case E is
        when UpPressed       => Up_Held := True;
        when UpReleased      => Up_Held := False;

        when DownPressed     => Down_Held := True;
        when DownReleased    => Down_Held := False;

        when LeftPressed     => Left_Held := True;
        when LeftReleased    => Left_Held := False;

        when RightPressed    => Right_Held := True;
        when RightReleased   => Right_Held := False;

        when LineBlack       => On_Black := True;
        when LineWhite       => On_Black := False;
      end case;

      Apply_Motion;

      exit when simulation_stopped;
    end loop;
  end MotorControlTask;

  --------------------------
  -- Background procedure --
  --------------------------
  procedure Background is
  begin
    while not simulation_stopped loop
      delay 0.25;
    end loop;
  end Background;

end Tasks;
