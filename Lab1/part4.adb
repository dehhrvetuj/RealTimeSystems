with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Float_Random;

use Ada.Text_IO;
use Ada.Integer_Text_IO;
use Ada.Numerics.Float_Random;

procedure Part4 is

   ------------------------------------------------------------------
   Size : constant Positive := 10;
   -- Fixed-size FIFO buffer

   type Buffer_Array is array (1 .. Size) of Integer;

   ------------------------------------------------------------------
   --  Protected FIFO Buffer
   ------------------------------------------------------------------
   protected Buffer is
      entry Put (Item : in  Integer);     -- Insert an item (may block)
      entry Get (Item : out Integer);     -- Remove an item (may block)
      procedure Stop;                     -- Signal termination
   private
      Data       : Buffer_Array;
      Head       : Positive   := 1;        -- Read position
      Tail       : Positive   := 1;        -- Write position
      Count      : Natural := 0;           -- Number of items stored
      Stopped    : Boolean := False;       -- Stop flag
   end Buffer;

   protected body Buffer is

      entry Put (Item : in Integer) when (Count < Size) is
      -- Put is open only when buffer is not full
      begin
         if not Stopped then
            Data (Tail) := Item;

            if Tail = Size then
               Tail := 1;
            else
               Tail := Tail + 1;
            end if;

            Count := Count + 1;
         else
            -- Ignore new items after Stop
            null;
         end if;
      end Put;

      entry Get (Item : out Integer) when Count > 0 is
      -- Get allowed only when there is data available

      begin
         Item := Data (Head);

         if Head = Size then
            Head := 1;
         else
            Head := Head + 1;
         end if;

         Count := Count - 1;
      end Get;

      procedure Stop is
      -- Mark the buffer as stopped
      begin
         Stopped := True;
      end Stop;

   end Buffer;

   ------------------------------------------------------------------
   --  Producer task
   ------------------------------------------------------------------
   task Producer is
      entry Stop;   -- External request to terminate
   end Producer;

   task body Producer is
      Gen     : Generator;       -- Random generator
      Value   : Integer;         -- Produced value
      Stopped : Boolean := False;
   begin
      Reset (Gen);

      loop
         -- Non-blocking check for Stop signal
         select
            accept Stop do
               Stopped := True;
            end Stop;
         or
            delay 0.0;
         end select;

         exit when Stopped;

         -- Produce a random number in 0..20
         Value := Integer (Random (Gen) * 21.0);

         -- Irregular production interval
         delay Duration (0.1 + Random (Gen) * 0.9);

         Put_Line ("Producer: produced value" & Integer'Image (Value));

         Buffer.Put (Value);   -- May block if buffer is full

         Put_Line ("Producer: inserted value" & Integer'Image (Value));
      end loop;

      Put_Line ("Producer: terminated.");
   end Producer;

   ------------------------------------------------------------------
   --  Consumer task
   ------------------------------------------------------------------
   task Consumer;

   task body Consumer is
      Gen    : Generator;   -- Random generator
      Value  : Integer;     -- Retrieved value
      Sum    : Integer := 0;
   begin
      Reset (Gen);

      loop
         -- Irregular consumption interval
         delay Duration (0.5 + Random (Gen) * 2.0);

         Put_Line ("Consumer: trying to get an item");

         Buffer.Get (Value);   -- May block if buffer is empty

         Put_Line ("Consumer: got value" & Integer'Image (Value));

         Sum := Sum + Value;

         if Sum > 100 then
            -- Termination condition reached
            Put_Line ("Consumer: sum > 100, terminating.");

            Buffer.Stop;     -- Stop the buffer
            Producer.Stop;   -- Ask producer to terminate

            exit;
         end if;
      end loop;

      Put_Line ("Consumer: terminated.");
   end Consumer;

begin
   Put_Line ("--- Part 4: Producer-Consumer with Protected Buffer ---");
   Put_Line ("Main: waiting for tasks...");
   Put_Line ("Main: finished.");
end Part4;
