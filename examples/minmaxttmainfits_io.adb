

with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line;


with MinmaxTT;

--with Numeric_Type;
--with Array_IO;
with FITS_IO;
with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;

with File;
with Optional;
with Optional.Reserved;
with Header;

procedure minmaxttmainfits_io
is

    package TIO renames Ada.Text_IO;
    package SIO renames Ada.Streams.Stream_IO;
    package CLI renames Ada.Command_Line;

 type I_Arr is array(SIO.Positive_Count range <>) of Integer;
 package FIO  is new FITS_IO(Integer, I_Arr);

 --package MMTT is new MinmaxTT(Float, Float'Last, Float'First);
 package MMTT is new MinmaxTT(Integer, Integer'Last, Integer'First);

 InFile   : SIO.File_Type;
 HDUStart : SIO.Positive_Count := 1;
 IArr : I_Arr(1 .. 10);

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

            FIO.Read(InFile, IArr);
            --AIO.Read(InFile, 0.0, 10.0, FArr);

        end;
    end;

    for I in IArr'Range
    loop
        MMTT.Minmax(IArr(I));
        TIO.Put(" " & Integer'Image(IArr(I)));
    end loop;
    TIO.New_Line;

--    TIO.Put_Line("Min " & Float'Image(MMTT.Min));
--    TIO.Put_Line("Max " & Float'Image(MMTT.Max));
    TIO.Put_Line("Min " & Integer'Image(MMTT.Min));
    TIO.Put_Line("Max " & Integer'Image(MMTT.Max));


    SIO.Close(InFile);



end minmaxttmainfits_io;


