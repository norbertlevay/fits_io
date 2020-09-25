--
-- Example create & write small FITS file
-- "small" meaning data (and header) fit into memory (heap).

-- FIXME SIO.Create(): check behaviour AdaRM: overwrites if file already exists ?
-- FIXME if AdaRM says SIO.Create guarantees File Index
-- to be 1 after Create ? Otherwise call Set_Index(File,1)

 -- For Streams see:
 -- https://en.wikibooks.org/wiki/Ada_Programming/Libraries/Ada.Streams.Stream_IO
 -- good, but misleading is this: 
 --
 -- Date_Rec'Write(Date_Stream, Some_Date)
 --
 -- the code used the generic ada-root-stream not specialization of any kind; so
 -- more correct would be:
 --
 -- Date_Rec'Write(Out_Stream, Some_Date)
 --

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Directories;

with V3_Types;        use V3_Types;
with Keyword_Record;  use Keyword_Record; -- FPositive needed
with Mandatory;       use Mandatory;      -- NAXISn_Arr needed
with Optional;       -- use Optional;       -- Card_Arr needed
with Optional.Reserved; use Optional.Reserved;
with Header;          use Header;         -- Create_Card needed

with Image;

with Numeric_Type;
with Array_IO;
with File.Misc;-- Write_Padding needed

with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;

with Buffer_Type;

with Ada.Strings.Bounded;


procedure create
is

 package TIO renames Ada.Text_IO;
 package SIO renames Ada.Streams.Stream_IO;

 -- specify Target-Type, why here ? FIXME handle as [A,B,Target_BITPIX] triple in Data
 --Target_BITPIX : Integer := Short_Integer'Size;
 --MandCards : Optional.Card_Arr := F32_Header.To_Cards(Target_BITPIX);
 -- FIXME Tf = Short_Integer, how to specify: with Buffer_Type unknown here! Raw.BITPIX
 -- cfitsio makes here "automatic type conversion"
 --
 -- FIXME It is hardoceded in Buffet_Type'Write 



 ColLength : constant SIO.Positive_Count := 256;
 RowLength : constant SIO.Positive_Count := 456;
 NAXISn    : NAXIS_Arr := (ColLength, RowLength);

 package F32_Header is new Image(Float_32, NAXISn);
 Mandatory_Keys : F32_Header.Image_Rec := (NAXISn'Length, -(Float_32'Size), NAXISn);

 -- add some optional/reserved cards

 type Valued_Key_Record_Arr is array (Integer range <>) of Optional.Valued_Key_Record;
 use Optional.BS70;
 ArrKeyRecs : Valued_Key_Record_Arr :=
     (
     (BZERO,    1*    "0.0"),
     (BSCALE,   1*    "1.0"),
     (BLANK,    1* "-32768"),
     (DATAMIN,  1*      "0"),
     (DATAMAX,  1*    "255")
     );


 -- prepare write data

 package F32_Data is new Buffer_Type(Float);
 Current_F32Column : F32_Data.Buffer(1..ColLength);


function Generate_Data(R : SIO.Count; ColLength : SIO.Positive_Count) return F32_Data.Buffer
is
    Col : F32_Data.Buffer(1..ColLength);
begin
 for I in Col'Range loop Col(I) := Float(I)-1.0; end loop;
 return Col;
end Generate_Data;


 -- FITS-file name

 Temp_File_Name  : constant String := Command_Name & ".fits.part";
 File_Name       : constant String := Command_Name & ".fits";
 Out_File   : SIO.File_Type;
 Out_Stream : SIO.Stream_Access;

begin

 SIO.Create (Out_File, SIO.Out_File, Temp_File_Name);
 Out_Stream := SIO.Stream(Out_File);

 Put_Line("Writing: " & Temp_File_Name); 

 -- write Header

 Header.Write_Card_SIMPLE(Out_File, True);
 F32_Header.Image_Rec'Write(Out_Stream, Mandatory_Keys);
 Valued_Key_Record_Arr'Write(Out_Stream, ArrKeyRecs);
 Header.Close(Out_File);

-- write Data Unit

 for I in 1 .. RowLength
 loop

     Current_F32Column := Generate_Data(I, ColLength);

     F32_Data.Buffer'Write(Out_Stream, Current_F32Column);

 end loop;
 File.Misc.Write_Padding(Out_File, SIO.Index(Out_File), File.Misc.DataPadValue);

-- succesfully written, close and rename

 SIO.Close(Out_File);

 Ada.Directories.Rename(Temp_File_Name, File_Name);

 Put_Line("Ready  : " & File_Name); 

exception
  when Except_ID : others => Put_Line(Standard_Error, Exception_Information(Except_ID));
end create;

