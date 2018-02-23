
with
    Ada.Exceptions,
    Ada.Text_IO,
    Ada.Text_IO.Bounded_IO,
    Ada.Text_IO.Text_Streams,
    Ada.Streams.Stream_IO,
    Ada.Command_Line,
    Ada.Strings.Unbounded,
    Ada.Strings.Bounded,
    Ada.Strings.Fixed,
    Ada.Unchecked_Conversion,
    System,
    Interfaces,
    GNAT.Traceback.Symbolic;

use
    Ada.Exceptions,
    Ada.Text_IO,
    Ada.Strings.Unbounded,
    Ada.Strings.Bounded,
    Ada.Streams,
    Ada.Streams.Stream_IO,
    System,
    Ada.Command_Line;

with FITS;      use FITS;
with FITS.Size; use FITS.Size;
with FITS.File; use FITS.File;
with FITS.Data; use FITS.Data;

with Interfaces;
use  Interfaces;


procedure exampleCreateFitsFile is

 Name   : String   := Command_Name & ".fits";
 FitsFile : SIO.File_Type;
 Cnt  : Positive := 5;
-- Data : DataArray_Type(Card,Cnt);
-- Card : Card_Type;
-- BITPIXVal : Integer;
 HDUNum  : Positive := 1;
 HDUSize : HDU_Size_Type;
 DUStart : SIO.Count;
begin
 New_Line;
 Put_Line("Usage " & Command_Name );
 Put_Line(" output: " & Name );
 New_Line(2);

 SIO.Open (FitsFile, SIO.In_File, Name);

 Set_Index(FitsFile,HDUNum);
 -- FIXME if AdaRM says SIO.Open guarantees File Index
 -- to be 1 after Open this line is not necessary


 declare
   dt    : Data_Type := To_DataType(HDUSize.DUSizeKeyVals.BITPIX);
   DataD : Data_Arr(dt,4);
 begin
   Put_Line("> and read DataUnit of type: " & Data_Type'Image(dt));
   Data_Arr'Read (SIO.Stream(FitsFile), DataD);
   New_Line;
 end;

 -- reset to start of Data
 Set_Index(FitsFile, DUStart);
 declare
--  DataBlock : FITS.Block_IO.Float32Block_Arr;
 begin

--   FITS.Block_IO.Float32Block_Arr'Read(SIO.Stream(FitsFile), DataBlock);

--   for I in DataBlock'Range loop
--     Ada.Text_IO.Put( FITS.Data.Float_32'Image(DataBlock(I)) & " ");
--   end loop;
  null;
 end;


 SIO.Close(FitsFile);


 exception

  when Except_ID : others =>
     declare
      Error :  Ada.Text_IO.File_Type := Standard_Error;
     begin
      New_Line(Error);
      Put_Line(Error, "Program error, send bug-report.");
--      New_Line(Error);
--      Put_Line(Error, "Exception_Name: " & Exception_Name( Except_ID ) );
--      Put_Line(Error, "Exception_Message: " & Exception_Message( Except_ID ) );
      Put_Line(Error, "Exception_Information: ");
      Put_Line(Error, Exception_Information( Except_ID ) );
      New_Line(Error);
      Put_Line(Error, "Call stack traceback symbols: addr2line -e ./fits addr1 addr2 ...");
      Put_Line(" > Trace-back of call stack: " );
      Put_Line( GNAT.Traceback.Symbolic.Symbolic_Traceback(Except_ID) );
      -- See more at: http://compgroups.net/comp.lang.ada/gnat-symbolic-traceback-on-exceptions/1409155#sthash.lNdkTjq6.dpuf
      -- Do the same manually, use:
      -- addr2line -e ./fits addr1 addr2 ...
     end;
end exampleCreateFitsFile;

