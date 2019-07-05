

with Ada.Streams.Stream_IO;

package FITSlib.File is

   package SIO renames Ada.Streams.Stream_IO;



   procedure Read_HDU (FitsFile : in SIO.File_Type);
   -- only experimental with debug prints

end FITSlib.File;
