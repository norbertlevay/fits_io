
with
    Build_Date,
    Commands,
    Ada.Exceptions,
    Ada.Text_IO,
    Ada.Direct_IO,
    Ada.Text_IO.Bounded_IO,
    Ada.Command_Line,
    Ada.Strings.Unbounded,
    Ada.Strings.Bounded,
    Ada.Streams.Stream_IO,
    System,
    Interfaces,
    GNAT.Traceback.Symbolic;


use
    Commands,
    Ada.Exceptions,
    Ada.Text_IO,
    Ada.Strings.Unbounded,
    Ada.Strings.Bounded,
    Ada.Command_Line;

with FITSStream; use FITSStream;


procedure fitsstreamtest
is
 FitsFile : Ada.Streams.Stream_IO.File_Type;
 Inx1 : Ada.Streams.Stream_IO.Count;
 Inx2 : Ada.Streams.Stream_IO.Count;
 Name : String    := "test.fits";

-- Cnt  : Positive := 80*5;
-- Data : DataArray_Type(int8,6);
-- Data : DataArray_Type(Char,Cnt);
 Cnt  : Positive := 10;
 Data : DataArray_Type(Card,Cnt);

begin

 Ada.Text_IO.Put_Line("Open file " & Name & " and read some chars...");
-- ----------------------------------------

 Ada.Streams.Stream_IO.Open (FitsFile, Ada.Streams.Stream_IO.In_File, Name);

 inx1 := Ada.Streams.Stream_IO.Index(FitsFile);
 Read (Ada.Streams.Stream_IO.Stream(FitsFile), Data);
 inx2 := Ada.Streams.Stream_IO.Index(FitsFile);

 for I in Positive range 1..Cnt loop
--   Ada.Text_IO.Put_Line(Positive'Image(I) & "> " & Interfaces.Integer_8'Image(Data.Int8Arr(I)));
--   Ada.Text_IO.Put(Data.CharArr(I));
   Ada.Text_IO.Put(Data.CardArr(I));
 end loop;

 Ada.Streams.Stream_IO.Close(FitsFile);

 Ada.Text_IO.Put_Line("Index before and after Read(): " & Inx1'Image & " " &  Inx2'Image );

-- ---------------------------------------

 --
 -- error handling
 --
 exception
  when Except_ID : others =>
     declare
      Error :  Ada.Text_IO.File_Type := Standard_Error;
     begin
      Put_Line(Error, "Program error, send a bug-report:");
      New_Line(Error);
      Put_Line(Error, "Exception_Name: " & Exception_Name( Except_ID ) );
      Put_Line(Error, "Exception_Message: " & Exception_Message( Except_ID ) );
      Put_Line(Error, "Exception_Information: ");
      Put_Line(Error, Exception_Information( Except_ID ) );
      New_Line(Error);
      Put_Line(Error, "Call stack traceback symbols: addr2line -e ./fits addr1 addr2 ...");
      --Put_Line(" > Trace-back of call stack: " );
      -- Put_Line( GNAT.Traceback.Symbolic.Symbolic_Traceback(Except_ID) );
      -- See more at: http://compgroups.net/comp.lang.ada/gnat-symbolic-traceback-on-exceptions/1409155#sthash.lNdkTjq6.dpuf
      -- Do teh same manually, use:
      -- addr2line -e ./fits addr1 addr2 ...
     end;
end fitsstreamtest;

