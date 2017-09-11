
-- with Interfaces;
-- with Ada.Streams.Stream_IO;

package body FITSStream is

   procedure Read (FitsStream : in Ada.Streams.Stream_IO.Stream_Access;
                   Data       : in out DataArray_Type)
   is
   begin

        DataArray_Type'Read( FitsStream, Data );

   end Read;

   procedure Write (FitsStream : in Ada.Streams.Stream_IO.Stream_Access;
                    Data       : in DataArray_Type)
   is
   begin

        DataArray_Type'Write( FitsStream, Data );

   end Write;

end FITSStream;

