
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

with FitsFloat;      use FitsFloat;
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
 Data : DataArray_Type(Card,Cnt);

 Card : Card_Type;
 BITPIXVal : Integer;

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

begin

 New_Line;
 Put_Line("Usage ./testfits HDUNum file.fits");
 New_Line(2);

 Put_Line(">> Open file " & Name);
 Put_Line(">  and read some chars...");

 SIO.Open (FitsFile, SIO.In_File, Name);

 Set_Index(FitsFile,HDUNum);

 inx1 := SIO.Index(FitsFile);
 DataArray_Type'Read (SIO.Stream(FitsFile), Data);
 inx2 := SIO.Index(FitsFile);

 -- print by 80 columns
 -- and also pick BITPIX value for DU-print later
 for I in Positive range 1..Data.Length
 loop
   Card := Data.CardArr(I);
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
 Parse_HeaderBlocks(FitsFile,HDUSize);-- move behind the Header

 declare
   dt    : Data_Type := To_DataType(HDUSize.DUSizeKeyVals.BITPIX);
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

 --New_Line;
 Put_Line("> reset to HDU start - BigEndian Float32 by swapping bytes");

 Set_Index(FitsFile,HDUNum);
 Parse_HeaderBlocks(FitsFile,HDUSize);-- move behind the Header

 declare
  type MyFloat is new Float;
  type Arr4xU8 is array (1..4) of Interfaces.Unsigned_8;
  Val : MyFloat;
  Arr : Arr4xU8;

  function Arr_To_MyFloat is
    new Ada.Unchecked_Conversion(Source => Arr4xU8, Target => MyFloat);

  procedure SwapBytes(arr : in out Arr4xU8) is
   temp : Arr4xU8;
  begin
   temp(1) := arr(4);
   temp(2) := arr(3);
   temp(3) := arr(2);
   temp(4) := arr(1);
   arr := temp;
  end SwapBytes;

 begin
   for I in 1..4 loop
   --MyFloat'Read (SIO.Stream(FitsFile), Val);
   Arr4xU8'Read (SIO.Stream(FitsFile), Arr);
   SwapBytes(Arr);
   Val := Arr_To_MyFloat(Arr);
   Ada.Text_IO.Put(" " & MyFloat'Image(Val));
   end loop;
   Ada.Text_IO.New_Line;
 end; -- declare2

 Put_Line("> reset to HDU start - BigEndian Float32 by defining own Float32");

 Set_Index(FitsFile,HDUNum);
 Parse_HeaderBlocks(FitsFile,HDUSize);-- move behind the Header

 declare
  DUStart : SIO.Count := Index(FitsFile);
  ValBE : FFloat32_BE;
--  ValLE : FFloat32_LE;
  type arrf is array(1..4) of FFloat32_BE;
     pragma Pack (arrf);


  af : arrf;

  Valf : Float;
 begin
   Ada.Text_IO.Put(" CompSize " & Integer'Image(arrf'Component_Size));
   Ada.Text_IO.Put(" VaSize   " & Integer'Image(FFloat32_BE'VAlue_Size));
   Ada.Text_IO.Put(" ObjSize  " & Integer'Image(FFloat32_BE'Object_Size));
   Ada.Text_IO.Put(" Size  " & Integer'Image(FFloat32_BE'Size));
   Ada.Text_IO.Put_Line("  SizeA " & Integer'Image(arrf'Size));


   Ada.Text_IO.Put("Index  " & SIO.Count'Image(Index(FitsFile)));
   arrf'Read (SIO.Stream(FitsFile), af);
   Ada.Text_IO.Put_Line(" --> " & SIO.Count'Image(Index(FitsFile)));

   Set_Index(FitsFile,DUStart);

   for I in 1..4 loop

    SIO.Set_Index(FitsFile,DUStart);
    Ada.Text_IO.Put("Index  " & SIO.Count'Image(Index(FitsFile)));
    FFloat32_BE'Read (SIO.Stream(FitsFile), ValBE);
    DUStart := DUStart + 4;

    Valf := FFloat32BE_To_Float(af(I));
    Ada.Text_IO.Put_Line(" " & Float'Image(Valf));
   end loop;
   Ada.Text_IO.New_Line;
 end; -- declare2

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
end testfits;

