

with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line;


with MinmaxTT;

with Numeric_Type;
with Array_IO;
with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;

with File;
with Optional;
with Optional.Reserved;
with Header;

with Ada.Sequential_IO;

procedure minmaxttmain
is

    package TIO renames Ada.Text_IO;
    package SIO renames Ada.Streams.Stream_IO;
    package CLI renames Ada.Command_Line;


 -- BEGIN : FITS_IO API considerations
 -- also note: these FITS_*_IO packages below could be use
 -- in Array_IO instead of Ada.Streams 'Write 'Read (used now)
 -- OR the unused: serialization + Strems.Stream_IO Read/Write(Stream_Element_Array)

 subtype Card_Type is String(1 .. 80);
 type Card_Arr is array (Natural range <>) of Card_Type;
 package FITS_Header_IO is new Ada.Sequential_IO(Element_Type => Card_Arr);

 generic
 type T (<>) is private;
 package FITS_IO_exp is
-- type Float_Arr is array (Natural range <>) of Float;
 package FITS_Data_Unit_IO is new Ada.Sequential_IO(Element_Type => T);
 end FITS_IO_exp;

 -- should FITS_IO generic type be:
 -- type Element_Type (<>) is private; <- like in sequential, allows for arrays
 -- END


 package Phys is new Numeric_Type(Integer);
 package Raw  is new Numeric_Type(Float);
 package AIO  is new Array_IO(Raw,Phys);

 --package MMTT is new MinmaxTT(Float, Float'Last, Float'First);
 package MMTT is new MinmaxTT(Integer, Integer'Last, Integer'First);

 InFile   : SIO.File_Type;
 HDUStart : SIO.Positive_Count := 1;

 IArr : array(SIO.Positive_Count range 1 .. 10) of Integer;
 FArr : Phys.Numeric_Arr(1 .. 10);

begin

    if(CLI.Argument_Count /= 1 ) 
    then 
      TIO.Put_Line("Usage  " & CLI.Command_Name & " <file name>");
      return;
    else
      SIO.Open(InFile, SIO.In_File, (CLI.Argument(1)));
    end if;

    declare
        HDUInfo : File.HDU_Info_Type := File.Read_Header(InFile);
    begin

        File.Set_File_Block_Index(InFile,HDUStart);

        declare
            Cards : Optional.Card_Arr :=
                Header.Read_Optional(InFile, Optional.Reserved.Reserved_Keys);
            UInValid : Boolean := False;
        begin

            AIO.Read(InFile, 0.0, 10.0, FArr);

        end;
    end;

    for I in FArr'Range
    loop
            IArr(I) := FArr(I);
        MMTT.Minmax(FArr(I));
        --TIO.Put(" " & Float'Image(FArr(I)));
        TIO.Put(" " & Integer'Image(FArr(I)));
    end loop;
    TIO.New_Line;

--    TIO.Put_Line("Min " & Float'Image(MMTT.Min));
--    TIO.Put_Line("Max " & Float'Image(MMTT.Max));
    TIO.Put_Line("Min " & Integer'Image(MMTT.Min));
    TIO.Put_Line("Max " & Integer'Image(MMTT.Max));


    SIO.Close(InFile);



end minmaxttmain;


