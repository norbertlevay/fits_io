
with
    Build_Date,
--    Commands,
    Ada.Exceptions,
    Ada.Text_IO,
    Ada.Direct_IO,
    Ada.Text_IO.Bounded_IO,
    Ada.Text_IO.Text_Streams,
    Ada.Command_Line,
    Ada.Strings.Unbounded,
    Ada.Strings.Bounded,
    Ada.Strings.Fixed,
    Ada.Streams.Stream_IO,
    System,
    Interfaces,
    GNAT.Traceback.Symbolic;


use
--    Commands,
    Ada.Exceptions,
    Ada.Text_IO,
    Ada.Strings.Unbounded,
    Ada.Strings.Bounded,
    Ada.Command_Line;

with FITS;      use FITS;
with FITS.File; use FITS.File;


procedure testfits
is
 StdoutStream : Ada.Text_IO.Text_Streams.Stream_Access := Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Output);


 FitsFile : Ada.Streams.Stream_IO.File_Type;
 Inx1 : Ada.Streams.Stream_IO.Count;
 Inx2 : Ada.Streams.Stream_IO.Count;
 Name : String    := "testfile.fits";

-- Cnt  : Positive := 80*5;
-- Data : DataArray_Type(int8,6);
-- Data : DataArray_Type(Char,Cnt);

 Cnt  : Positive := 5;
 Data : DataArray_Type(Card,Cnt);

-- DataD : DataArray_Type(Int8,4);

-- Cnt  : Positive := 2;
-- Data : DataArray_Type(HBlock,Cnt);


  procedure Print_HDUSize (Index : Positive; HDUSize : HDU_Size_Type)
  is
      FreeSlotCnt : Natural;
  begin
       -- calc free slots
       FreeSlotCnt := 36 - (HDUSize.CardsCnt mod 36);
       -- mod is 0 when Block has 36 cards e.g. is full
       if FreeSlotCnt = 36 then
        FreeSlotCnt := 0;
       end if;

       Ada.Text_IO.Put("HDUVect HDU#" & Integer'Image(Index) );
       Ada.Text_IO.Put("   Cards: " &
                       Ada.Strings.Fixed.Tail(Integer'Image(HDUSize.CardsCnt),5,' ') &
                       " EmptyCardSlots: " &
                       Ada.Strings.Fixed.Tail(Integer'Image( FreeSlotCnt ),2,' ') );

       if HDUSize.DUSizeParam.Naxes > 0 then
        Ada.Text_IO.Put("   Data: "  & Ada.Strings.Fixed.Head( FitsData_Type'Image(HDUSize.DUSizeParam.Data),8,' ') );
        Ada.Text_IO.Put(" ( ");
        for J in 1 .. (HDUSize.DUSizeParam.Naxes - 1)
         loop
          Ada.Text_IO.Put(FPositive'Image(HDUSize.DUSizeParam.Naxis(J)) & " x " );
        end loop;
        Ada.Text_IO.Put(FPositive'Image(HDUSize.DUSizeParam.Naxis(HDUSize.DUSizeParam.Naxes)));
        Ada.Text_IO.Put_Line(" ) ");
       end if;
  end Print_HDUSize;

  procedure PutFITSData (Data : in DataArray_Type)
  is
  begin
   for I in Positive range 1 .. Data.Length
   loop
    case Data.FitsType is
    when Int8 =>
     Ada.Text_IO.Put( Interfaces.Integer_8'Image(Data.Int8Arr(I)) & " ");
    when Int16 =>
     Ada.Text_IO.Put( Interfaces.Integer_16'Image(Data.Int16Arr(I)) & " ");
    when Int32 =>
     Ada.Text_IO.Put( Interfaces.Integer_32'Image(Data.Int32Arr(I)) & " ");
    when Int64 =>
     Ada.Text_IO.Put( Interfaces.Integer_64'Image(Data.Int64Arr(I)) & " ");
    when Float32 =>
     Ada.Text_IO.Put( Interfaces.IEEE_Float_32'Image(Data.Float32Arr(I)) & " ");
    when Float64 =>
     Ada.Text_IO.Put( Interfaces.IEEE_Float_64'Image(Data.Float64Arr(I)) & " ");
    when others =>
      null; -- FIXME exception or ?
    end case;
   end loop;
   Ada.Text_IO.New_Line;
  end PutFITSData;


   HDUNum : Positive := 1;
   Card : Card_Type;
   BITPIXVal : Integer;

begin

  Ada.Text_IO.Put_Line("Usage ./testfits HDUNum ");
  Ada.Text_IO.Put_Line("HDUNum >" &Integer'Image(Integer'Value(Argument(1))));
  HDUNum :=Integer'Value( Argument(1) );

-- ----------------------------------------
 Ada.Text_IO.New_Line;
 Ada.Text_IO.Put_Line("Open file " & Name & " and read some chars...");

 Ada.Streams.Stream_IO.Open (FitsFile, Ada.Streams.Stream_IO.In_File, Name);

 Set_Index(FitsFile,HDUNum,Data.FitsType);

 inx1 := Ada.Streams.Stream_IO.Index(FitsFile);
 DataArray_Type'Read (Ada.Streams.Stream_IO.Stream(FitsFile), Data);
 inx2 := Ada.Streams.Stream_IO.Index(FitsFile);

 DataArray_Type'Write(StdoutStream, Data);
 Ada.Text_IO.New_Line;
 Ada.Text_IO.New_Line;

 for I in Positive range 1..Data.Length
-- for I in Data.CardArr'Range
 loop
--   Ada.Text_IO.Put_Line(Positive'Image(I) & "> " & Interfaces.Integer_8'Image(Data.Int8Arr(I)));
--   Ada.Text_IO.Put(Data.CharArr(I));
   Card := Data.CardArr(I);
   Ada.Text_IO.Put_Line(Card);

     if    (Card(1..9) = "BITPIX  =") then
       BITPIXVal := Integer'Value(Card(10..30));
     end if;

--   for J in 1..CardsCntInBlock loop
--   Ada.Text_IO.Put_Line(Data.HBlockArr(I)(J));
--   end loop;
 end loop;

 --
 -- dynamically create Data of type as given in Header/BITPIX
 --
 declare
   dt    : FitsData_Type := To_FitsDataType (BITPIXVal);
   DataD : DataArray_Type(dt,4);
 begin
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line("and read DataUnit...");

   Set_Index(FitsFile,HDUNum,DataD.FitsType,10*4);
   DataArray_Type'Read (Ada.Streams.Stream_IO.Stream(FitsFile), DataD);
   PutFITSData(DataD);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put("'Write(Stdout,DataD): >>");
   DataArray_Type'Write(StdoutStream, DataD);
   Ada.Text_IO.Put_Line("<<");

 end; -- declare

 Ada.Streams.Stream_IO.Close(FitsFile);
-- Ada.Text_IO.Put_Line("Index before and after Read(): " & Inx1'Image & " " &  Inx2'Image );


 ------------------------------------------------
 Ada.Text_IO.New_Line;
 Ada.Text_IO.Put_Line("Open file " & Name & " and List_Content...");

 Ada.Streams.Stream_IO.Open (FitsFile, Ada.Streams.Stream_IO.In_File, Name);

 List_Content(FitsFile,Print_HDUSize'Access);

 Ada.Streams.Stream_IO.Close(FitsFile);

-- Limits:

 Ada.Text_IO.Put_Line("Max NAXIS  : " & Positive'Image(MaxAxes));
 Ada.Text_IO.Put_Line("Max NAXISn : " & FPositive'Image(FPositive'Last));
 Ada.Text_IO.Put_Line("Max File size     : " & Ada.Streams.Stream_IO.Count'Image(Ada.Streams.Stream_IO.Positive_Count'Last));
 Ada.Text_IO.Put_Line("Max DataUnit size : ???" );
 Ada.Text_IO.Put_Line("Supported only machines with wordsize not bigger then min(BITPIX)=8 and divisible." );



-- ---------------------------------------

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
      --Put_Line(" > Trace-back of call stack: " );
      -- Put_Line( GNAT.Traceback.Symbolic.Symbolic_Traceback(Except_ID) );
      -- See more at: http://compgroups.net/comp.lang.ada/gnat-symbolic-traceback-on-exceptions/1409155#sthash.lNdkTjq6.dpuf
      -- Do teh same manually, use:
      -- addr2line -e ./fits addr1 addr2 ...
     end;
end testfits;

