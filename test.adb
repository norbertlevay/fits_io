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

with Fits_IO; use Fits_IO;
--with HDU;


procedure test
is
 --
 -- count lines in Header file
 --
 function NoOfLines( FileName : in String )
  return Positive
 is
  FileHandle     : Ada.Text_IO.File_Type;
  NoLines : Natural := 0;
  ENDFound : Boolean := False;
 begin
    Ada.Text_IO.Open( FileHandle, Ada.Text_IO.In_File, FileName );
    loop
      exit when ENDFound;
        declare
         Dummy : String := Ada.Text_IO.Get_Line( FileHandle );
         -- raises exception if no END-Line found but EOF reached
        begin
         if Dummy'Length >= 3 then
           ENDFound := (Dummy(1..3) = "END");
         end if;
        end;
        NoLines := NoLines + 1;
    end loop;
    Ada.Text_IO.Close(FileHandle);
  return NoLines;
 end NoOfLines;
 --
 -- Convert Header from text file into Header_Type
 --
 function Read_HeaderFromTextFile( FileName : in String )
  return Header_Type
 is
   FileHandle : Ada.Text_IO.File_Type;
   NoLines : Positive := NoOfLines(FileName); -- constraint exception if NoLines = 0
   Header  : Header_Type(1 .. NoLines);
 begin
   Ada.Text_IO.Open( FileHandle, Ada.Text_IO.In_File, FileName );
   for I in Header'Range loop
      Header(I) := Card.To_Bounded_String(Ada.Text_IO.Get_Line( FileHandle ));
   end loop;
   Ada.Text_IO.Close(FileHandle);
   return Header;
 end Read_HeaderFromTextFile;


 Fits : Fits_IO.File_Type;
 Mode : Fits_IO.File_Mode := Append_File;
 Name : String    := "test.fits";
 Header : Header_Type :=
  (Card.To_Bounded_String("BITPIX  = 8"),
   Card.To_Bounded_String("NAXIS   = 2"),
   Card.To_Bounded_String("NAXIS1  = 2"),
   Card.To_Bounded_String("NAXIS2  = 3"),
   Card.To_Bounded_String("END"));
 RealHeader : Header_Type := Read_HeaderFromTextFile("realheader.hdr");

 Data : DataArray_Type := (Option   => int8,
                           Length   => 6,
                         --  Int32Arr => (16#7FFFFFFF#,2,3,4,5,16#7FFFFFFF#));
                         --  Int16Arr => (16#7FFF#,2,3,4,5,16#7FFF#));
                           Int8Arr => (16#7F#,2,3,4,5,16#7F#));
                         --  Float32Arr => (1.0E1,1.0E2,1.0E3,1.0E4,1.0E5,1.0E6));

 Data2 : DataArray_Type(int8,6);

 -- just for print sizes
 DataInt8  : DataArray_Type(int8, 10);
 DataInt16 : DataArray_Type(int16,10);
 DataInt32 : DataArray_Type(int32,10);

begin

-- Ada.Text_IO.Put_Line("Name   " & System.Name);
 Ada.Text_IO.Put_Line("Default_Bit_Order " & System.Bit_Order'Image(System.Default_Bit_Order));
 Ada.Text_IO.Put_Line("Endianness   " & System.Bit_Order'Image(DataArray_Type'Bit_Order));
 Ada.Text_IO.Put_Line("Storage_Unit " & Integer'Image(System.Storage_Unit));
 Ada.Text_IO.Put_Line("Word_Size    " & Integer'Image(System.Word_Size));
-- not very useful [Ada] Ada.Text_IO.Put_Line("Memory_Size  " & Long_Long_Integer'Image(System.Memory_Size));

 Ada.Text_IO.Put_Line("Alignment       " & Integer'Image(DataArray_Type'Alignment));
 Ada.Text_IO.Put_Line("DataType   size " & Integer'Image(Data.Option'Size));
 Ada.Text_IO.Put_Line("Var-record size 10xInt8  " & Integer'Image(DataInt8'Size));
 Ada.Text_IO.Put_Line("     Array size 10xInt8  " & Integer'Image(DataInt8.Int8Arr'Size));
 Ada.Text_IO.Put_Line("Var-record size 10xInt16 " & Integer'Image(DataInt16'Size));
 Ada.Text_IO.Put_Line("     Array size 10xInt16 " & Integer'Image(DataInt16.Int16Arr'Size));
 Ada.Text_IO.Put_Line("Var-record size 10xInt32 " & Integer'Image(DataInt32'Size));
 Ada.Text_IO.Put_Line("     Array size 10xInt32 " & Integer'Image(DataInt32.Int32Arr'Size));

 Ada.Text_IO.Put_Line("int8  " & Integer'Image(Interfaces.Integer_8'Size));
 Ada.Text_IO.Put_Line("int16 " & Integer'Image(Interfaces.Integer_16'Size));
 Ada.Text_IO.Put_Line("int32 " & Integer'Image(Interfaces.Integer_32'Size));
 Ada.Text_IO.Put_Line("int64 " & Integer'Image(Interfaces.Integer_64'Size));
 Ada.Text_IO.Put_Line("float32 " & Integer'Image(Float'Size));
 Ada.Text_IO.Put_Line("float64 " & Integer'Image(Long_Float'Size));

 for I in 1..6 loop
  Data2.Int8Arr(I) := Interfaces.Integer_8(2*I);
 end loop;


 Create (Fits, Mode, Name);
-- Ada.Text_IO.Put_Line("Write Primary Header...");
-- Write (Fits, RealHeader);

 Ada.Text_IO.Put_Line("Write Header to 1st HDU...");
 Write (Fits, Header, 1);

 Ada.Text_IO.Put_Line("Write Data to 1st HDU...");
 Write (Fits,1, 1,Data2);

 Close(Fits);

 Ada.Text_IO.Put_Line("Re-open file and Read 1st HDU...");
 Open (Fits, In_File, Name);
 declare
  dr : DataArray_Type := Read (Fits,1,int8,1,6 );
 begin
  for I in 1..6 loop
   Ada.Text_IO.Put_Line(Integer'Image(I) & "> " & Interfaces.Integer_8'Image(dr.Int8Arr(I)));
  end loop;
 end;
 Close(Fits);

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
end test;

