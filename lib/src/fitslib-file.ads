

with Ada.Streams.Stream_IO;

package FITSlib.File is

   package SIO renames Ada.Streams.Stream_IO;



   procedure Read_HDU (FitsFile : in SIO.File_Type);
   -- only experimental with debug prints

  function Read_DataSize_bits (FitsFile : in SIO.File_Type)
           return Natural;




end FITSlib.File;
