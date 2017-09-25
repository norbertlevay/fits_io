
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
    Ada.Strings.Fixed,
--    Ada.Streams.Stream_IO,
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

with FITS_DIO; use FITS_DIO;
--with FITS_DIO.Content; use FITS_DIO.Content;


procedure testfits_dio
is
-- FitsFile : Ada.Streams.Stream_IO.File_Type;
 FitsFile : BIO.File_Type;
-- Inx1 : BIO.Count;
-- Inx2 : BIO.Count;
 Name : String    := "testfile.fits";

-- Cnt  : Positive := 80*5;
-- Data : DataArray_Type(int8,6);
-- Data : DataArray_Type(Char,Cnt);

-- Cnt  : Positive := 5;
-- Data : DataArray_Type(Card,Cnt);

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
        Ada.Text_IO.Put("   Data: "  & Ada.Strings.Fixed.Head( FITSData_Type'Image(HDUSize.DUSizeParam.Data),8,' ') );
        Ada.Text_IO.Put(" ( ");
        for J in 1 .. (HDUSize.DUSizeParam.Naxes - 1)
         loop
          Ada.Text_IO.Put(FPositive'Image(HDUSize.DUSizeParam.Naxis(J)) & " x " );
        end loop;
        Ada.Text_IO.Put(FPositive'Image(HDUSize.DUSizeParam.Naxis(HDUSize.DUSizeParam.Naxes)));
        Ada.Text_IO.Put_Line(" ) ");
       end if;
  end Print_HDUSize;


begin

 ------------------------------------------------
 Ada.Text_IO.Put_Line("Open file " & Name & " and List_Content...");

 BIO.Open (FitsFile, BIO.In_File, Name);

 List_Content(FitsFile,Print_HDUSize'Access);

 BIO.Close(FitsFile);

-- Limits:
 Ada.Text_IO.New_Line;
 Ada.Text_IO.Put_Line("Max NAXIS  : " & Positive'Image(MaxAxes));
 Ada.Text_IO.Put_Line("Max NAXISn : " & FPositive'Image(FPositive'Last));
 Ada.Text_IO.Put_Line("Max File size     : " & BIO.Count'Image(BIO.Positive_Count'Last));
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
end testfits_dio;

