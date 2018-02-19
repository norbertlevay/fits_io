
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

with FitsFloat;      use FitsFloat;
with FITS;      use FITS;
with FITS.File; use FITS.File;
with FITS.Data; use FITS.Data;

with FITS.Block_IO; --use FITS.File;

with PNG_IO.Base;
use  PNG_IO.Base;

with Interfaces;
use  Interfaces;



procedure testbio is

 StdoutStream : Ada.Text_IO.Text_Streams.Stream_Access := Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Output);

 HDUNum : Positive := Integer'Value( Argument(1) );
 Name   : String   := Argument(2);

 FitsFile : SIO.File_Type;
-- Inx1 : SIO.Count;
-- Inx2 : SIO.Count;

 Cnt  : Positive := 5;
-- Data : DataArray_Type(Card,Cnt);

-- Card : Card_Type;
-- BITPIXVal : Integer;

 HDUSize  : HDU_Size_Type;

  procedure PutFITSData (Data : in Data_Arr)
  is
  begin
   for I in Positive range 1 .. Data.Length
   loop
    case Data.FitsType is
    when UInt8 =>
     Ada.Text_IO.Put( FITS.Data.Unsigned_8'Image(Data.UInt8Arr(I)) & " ");
    when Int16 =>
     Ada.Text_IO.Put( FITS.Data.Integer_16'Image(Data.Int16Arr(I)) & " ");
    when Int32 =>
     Ada.Text_IO.Put( FITS.Data.Integer_32'Image(Data.Int32Arr(I)) & " ");
    when Int64 =>
     Ada.Text_IO.Put( FITS.Data.Integer_64'Image(Data.Int64Arr(I)) & " ");
    when Float32 =>
     Ada.Text_IO.Put( FITS.Data.Float_32'Image(Data.Float32Arr(I)) & " ");
    when Float64 =>
     Ada.Text_IO.Put( FITS.Data.Float_64'Image(Data.Float64Arr(I)) & " ");
    when others =>
      null; -- FIXME exception or ?
    end case;
   end loop;
   Ada.Text_IO.New_Line;
  end PutFITSData;


  DUStart : SIO.Count;
begin

 New_Line;
 Put_Line("Usage ./testbio HDUNum file.fits");
 New_Line(2);

 Put_Line(">> Open file " & Name);
 Put_Line(">  and read some chars...");

 SIO.Open (FitsFile, SIO.In_File, Name);

 Set_Index(FitsFile,HDUNum);

 Parse_HeaderBlocks(FitsFile,HDUSize);-- move behind the Header
 DUStart := Index(FitsFile);

 declare
   dt    : Data_Type := To_DataType(HDUSize.DUSizeKeyVals.BITPIX);
   DataD : Data_Arr(dt,4);
 begin
   Put_Line("> and read DataUnit of type: " & Data_Type'Image(dt));
   Data_Arr'Read (SIO.Stream(FitsFile), DataD);
   PutFITSData(DataD);
   New_Line;
 end;

 -- reset to start of Data
 Set_Index(FitsFile, DUStart);
 declare
  DataBlock : FITS.Block_IO.Float32Block_Arr;
 begin

   FITS.Block_IO.Float32Block_Arr'Read(SIO.Stream(FitsFile), DataBlock);

   for I in DataBlock'Range loop
     Ada.Text_IO.Put( FITS.Data.Float_32'Image(DataBlock(I)) & " ");
   end loop;
 end;


 SIO.Close(FitsFile);

 ------------------
 -- Print Limits --
 ------------------
 New_Line(2);
 Put_Line(">> Print Limits:");

 Put_Line("Max NAXIS         : " & Positive'Image(MaxAxes));
 Put_Line("Max NAXISn        : " & FPositive'Image(FPositive'Last));
 Put_Line("Max File size     : " & SIO.Count'Image(SIO.Positive_Count'Last));
 Put_Line("Max DataUnit size = Max File size - 1 block (header) " );
 Put_Line("The code supports machines with wordsize = min(BITPIX) = 8." );
 Put_Line("16bit machines (often some DSP's) not supported: code to pack/unpack two 8-bit FITS characters into 16bit words not implemented." );

 --
 -- error handling
 --
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
end testbio;

