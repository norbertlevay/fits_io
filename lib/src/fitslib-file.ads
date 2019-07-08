

with Ada.Streams.Stream_IO;
with FITSlib.Header; use FITSlib.Header; -- HDU_Variant needed

package FITSlib.File is

   package SIO renames Ada.Streams.Stream_IO;


   -- determine type of the current HDU
   function Peek (File : in SIO.File_Type) return HDU_Variant;
   -- Peek will not move File Index

   procedure Read_HDU (FitsFile : in SIO.File_Type);
   -- only experimental with debug prints

  function Read_DataSize_bits (FitsFile : in SIO.File_Type)
           return Natural;




end FITSlib.File;
