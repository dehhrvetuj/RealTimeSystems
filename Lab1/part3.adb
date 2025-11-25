with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Float_Random;

use Ada.Text_IO;
use Ada.Integer_Text_IO;
use Ada.Numerics.Float_Random;

procedure Part3 is

   ------------------------------------------------------------------
   --  Buffer task: FIFO queue with blocking rendezvous
   ------------------------------------------------------------------
   task Buffer is
      entry Put (Item : in Integer);    -- Insert an item
      entry Get (Item : out Integer);   -- Retrieve an item
      entry Stop;                       -- Stop the buffer
   end Buffer;

   ------------------------------------------------------------------
   --  Producer task
   ------------------------------------------------------------------
   task Producer is
      entry Stop;                       -- Request termination
   end Producer;

   ------------------------------------------------------------------
   --  Consumer task
   ------------------------------------------------------------------
   task Consumer;

   ------------------------------------------------------------------
   --  Buffer implementation
   ------------------------------------------------------------------
   task body Buffer is
      Size : constant Positive := 10;

      Data   : array (1 .. Size) of Integer;   -- FIFO storage
      Head   : Positive := 1;                  -- Read index
      Tail   : Positive := 1;                  -- Write index
      Count  : Natural  := 0;                  -- Items stored
      Stopped : Boolean := False;              -- Stop flag

   begin
      loop
         select
            --------------------------------------------------------
            -- Get allowed only when the buffer is not empty
            --------------------------------------------------------
            when Count > 0 =>
               accept Get (Item : out Integer) do
                  Item := Data (Head);

                  if Head = Size then
                     Head := 1;            -- Wrap around
                  else
                     Head := Head + 1;
                  end if;

                  Count := Count - 1;       -- One item removed
               end Get;

         or
            --------------------------------------------------------
            -- Put allowed when buffer is not full AND not stopped
            --------------------------------------------------------
            when (Count < Size) and then (not Stopped) =>
               accept Put (Item : in Integer) do
                  Data (Tail) := Item;

                  if Tail = Size then
                     Tail := 1;            -- Wrap around
                  else
                     Tail := Tail + 1;
                  end if;

                  Count := Count + 1;       -- One item added
               end Put;

         or
            --------------------------------------------------------
            -- Stop request: disable further Put operations
            --------------------------------------------------------
            accept Stop do
               Stopped := True;
            end Stop;

            exit;                           -- Leave buffer loop
         end select;
      end loop;
   end Buffer;

   ------------------------------------------------------------------
   --  Producer implementation
   ------------------------------------------------------------------
   task body Producer is
      Gen     : Generator;        -- Random generator
      Value   : Integer;          -- Produced value
      Stopped : Boolean := False; -- Local stop flag
   begin
      Reset (Gen);

      loop
         -- Non-blocking check for Stop
         select
            accept Stop do
               Stopped := True;
            end Stop;
         or
            delay 0.0;             -- Continue immediately
         end select;

         exit when Stopped;

         -- Generate random value 0..20
         Value := Integer (Float (Random (Gen) * 21.0));

         -- Random production delay
         delay Duration (0.1 + Float (Random (Gen) * 0.9));

         Put_Line ("Producer: produced value " & Integer'Image (Value));

         -- Blocks when the buffer is full
         Buffer.Put (Value);

         Put_Line ("Producer: successfully inserted value "
                   & Integer'Image (Value));
      end loop;

   exception
      when others =>
         Put_Line ("Producer: terminated.");
   end Producer;

   ------------------------------------------------------------------
   --  Consumer implementation
   ------------------------------------------------------------------
   task body Consumer is
      Gen    : Generator;   -- Random generator
      Value  : Integer;     -- Consumed value
      Sum    : Integer := 0;
   begin
      Reset (Gen);

      loop
         -- Random consumption delay
         delay Duration (0.5 + Float (Random (Gen) * 2.0));

         Put_Line ("Consumer: trying to get an item");

         -- Blocks when buffer is empty
         Buffer.Get (Value);

         Put_Line ("Consumer: successfully got value"
                   & Integer'Image (Value));

         Sum := Sum + Value;

         if Sum > 100 then
            Put_Line ("Consumer: sum above 100, terminating program.");

            Buffer.Stop;      -- Stop buffer
            Producer.Stop;    -- Stop producer

            exit;
         end if;
      end loop;
   end Consumer;

begin
   Put_Line ("--- Part 3: Producer-Consumer with Buffer Task ---");
   Put_Line ("Main: waiting for tasks to terminate...");
   Put_Line ("Main: finished.");
end Part3;
