
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
    Ada.Command_Line;

with FITS;      use FITS;
with FITS.Header; use FITS.Header;
with FITS.Size; use FITS.Size;
with FITS.File; use FITS.File;
with FITS.Data; use FITS.Data;

with PNG_IO.Base;
use  PNG_IO.Base;

with Interfaces;
use  Interfaces;



procedure testfits is

 StdoutStream : Ada.Text_IO.Text_Streams.Stream_Access := Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Output);

 HDUNum : Positive := Integer'Value( Argument(1) );
 Name   : String   := Argument(2);

 FitsFile : SIO.File_Type;
 Inx1 : SIO.Count;
 Inx2 : SIO.Count;

 Cnt  : Positive := 5;
-- Data : DataArray_Type(Card,Cnt);
 Data : Card_Arr(1..Cnt);

 Card : Card_Type;
 BITPIXVal : Integer;

  procedure PutFITSData (Data : in Data_Arr)
  is
  begin
   for I in FPositive range 1 .. Data.Length
   loop
    case Data.FitsType is
    when UInt8 =>
     Ada.Text_IO.Put( FITS.Unsigned_8'Image(Data.UInt8Arr(I)) & " ");
    when Int16 =>
     Ada.Text_IO.Put( FITS.Integer_16'Image(Data.Int16Arr(I)) & " ");
    when Int32 =>
     Ada.Text_IO.Put( FITS.Integer_32'Image(Data.Int32Arr(I)) & " ");
    when Int64 =>
     Ada.Text_IO.Put( FITS.Integer_64'Image(Data.Int64Arr(I)) & " ");
    when Float32 =>
     Ada.Text_IO.Put( FITS.Float_32'Image(Data.Float32Arr(I)) & " ");
    when Float64 =>
     Ada.Text_IO.Put( FITS.Float_64'Image(Data.Float64Arr(I)) & " ");
    when others =>
      null; -- FIXME exception or ?
    end case;
   end loop;
   Ada.Text_IO.New_Line;
  end PutFITSData;

begin

 New_Line;
 Put_Line("Usage ./testfits HDUNum file.fits");
 New_Line(2);

 Put_Line(">> Open file " & Name);
 Put_Line(">  and read some chars...");

 SIO.Open (FitsFile, SIO.In_File, Name);

 Set_Index(FitsFile,HDUNum);

 inx1 := SIO.Index(FitsFile);
 Card_Arr'Read (SIO.Stream(FitsFile), Data);
 inx2 := SIO.Index(FitsFile);

 -- print by 80 columns
 -- and also pick BITPIX value for DU-print later
 for I in Data'Range
 loop
   Card := Data(I);
   Ada.Text_IO.Put_Line(Card);
   if    (Card(1..9) = "BITPIX  =") then
     BITPIXVal := Integer'Value(Card(10..30));
   end if;
 end loop;

 --
 -- dynamically create Data of type as given in Header/BITPIX
 --

 New_Line;
 Put_Line("> reset to HDU start");

 Set_Index(FitsFile,HDUNum);
-- Parse_HeaderBlocks(FitsFile,HDUSize);-- move behind the Header

 declare
   HDUInfo : HDU_Info_Type := Get(FitsFile);
   dt    : Data_Type := To_DataType(HDUInfo.BITPIX);
   DataD : Data_Arr(dt,4);
 begin
   Put_Line("> and read DataUnit of type: " & Data_Type'Image(dt));
--   Set_Index(FitsFile,HDUNum,DataD.FitsType,10*4);
--   FIXME now that above call does not support Offset, how to move in DataUnit ?
   Data_Arr'Read (SIO.Stream(FitsFile), DataD);
   PutFITSData(DataD);
   New_Line;
--   DataArray_Type'Read (SIO.Stream(FitsFile), DataD);
--   PutFITSData(DataD);
   New_Line;
 end; -- declare1


 SIO.Close(FitsFile);

 ------------------
 -- Print Limits --
 ------------------
 New_Line(2);
 Put_Line(">> Print Limits:");

 Put_Line("Max NAXIS         : " & Positive'Image(NAXIS_Max));
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
end testfits;

