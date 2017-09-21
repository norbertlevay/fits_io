
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

with FITS; use FITS;


procedure fitsstreamtest
is
 FitsFile : Ada.Streams.Stream_IO.File_Type;
 Inx1 : Ada.Streams.Stream_IO.Count;
 Inx2 : Ada.Streams.Stream_IO.Count;
 Name : String    := "testfile.fits";

-- Cnt  : Positive := 80*5;
-- Data : DataArray_Type(int8,6);
-- Data : DataArray_Type(Char,Cnt);

 Cnt  : Positive := 3;
 Data : DataArray_Type(Card,Cnt);

-- Cnt  : Positive := 2;
-- Data : DataArray_Type(HBlock,Cnt);

 HDUArr : HDU_Info_Arr(1..2);
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
   Ada.Text_IO.Put_Line(Data.CardArr(I));
--   for J in 1..CardsCntInBlock loop
--   Ada.Text_IO.Put_Line(Data.HBlockArr(I)(J));
--   end loop;

 end loop;
 Ada.Streams.Stream_IO.Close(FitsFile);
 Ada.Text_IO.Put_Line("Index before and after Read(): " & Inx1'Image & " " &  Inx2'Image );

 ------------------------------------------------
 Ada.Text_IO.Put_Line("Now try List_Content...");

 Ada.Streams.Stream_IO.Open (FitsFile, Ada.Streams.Stream_IO.In_File, Name);

 List_Content(FitsFile,HDUArr);
 for I in HDUArr'Range
 loop
  Ada.Text_IO.Put_Line("HDU#" &
                        Integer'Image(I) &
                        Integer'Image(HDUArr(I).CardsCnt) );
 end loop;

 Ada.Streams.Stream_IO.Close(FitsFile);




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

