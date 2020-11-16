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
 -- NOTE on Buffer_Type set-up:
 -- specify Target-Type, why here ? FIXME handle as [A,B,Target_BITPIX] triple in Data
 -- MandCards : Optional.Card_Arr := F32_Header.To_Cards(Target_BITPIX);
 -- NOTE File_BITPIX: cfitsio makes here "automatic type conversion"
 --
 -- NOTE on Image metadata set-up:
 -- NOTE source data (Tm) is described in Image-data-model
 -- if we write cards, those must be cards of the target Tf
 -- Info needed to specify and translate to target type is
 -- basically the Optional.Reserved.Array_Keys :


with Ada.Text_IO;      --use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Directories;

with FITS_IO;           use FITS_IO;
with FITS_IO.Data_Unit;

with V3_Types;        use V3_Types;
with Keyword_Record;  use Keyword_Record; -- FPositive needed
--with Mandatory;       use Mandatory;      -- NAXISn_Arr needed
with Optional;       -- use Optional;       -- Card_Arr needed
with Optional.Reserved; use Optional.Reserved;
with Header;          use Header;         -- Create_Card needed

with Image;

with Numeric_Type;
with Array_IO;
with File.Misc;-- Write_Padding needed

with Pool_For_Numeric_Type; use Pool_For_Numeric_Type;

with Ada.Strings.Bounded;

with Init;


procedure create
is

    Zero  :  Float := 0.0;
    F_NaN : constant Float := 0.0/Zero;

    package TIO renames Ada.Text_IO;
    package SIO renames Ada.Streams.Stream_IO;

    -- Metadata

    ColLength : constant Positive_Count := 256;
    RowLength : constant Positive_Count := 456;

    subtype MD_Tm is Float;

    MD_NAXISn            : NAXIS_Array := (ColLength, RowLength);
    MD_Min               : Float     :=   0.0;
    MD_Max               : Float     := 255.0;
    MD_Unit              : String    := "Undef";
    MD_Undef_Valid       : Boolean   := True;
    MD_Undef_Value       : Float     := F_NaN;

    MD_Memory_BITPIX  : Integer := -(MD_Tm'Size);
                    -- FIXME use To_BITPIX(Dummy : MD_Tm) return Integer

    -- Inputs for Write

    In_File_DUType : Init.DU_Type := Init.Int16;
    --In_File_BITPIX : Integer := Short_Integer'Size; -- needed only for Image'Write
    In_A : Float := 0.0;
    In_B : Float := 1.0;

    -- F->I conversion: caller must supply Undef value
    -- for the file if data has undef values
    In_File_Undefined_Valid : Boolean := MD_Undef_Valid;
    In_File_Undefined_Value : Float   := Float(Short_Integer'Last);
                            -- FIXME convert File_BITPIX -> T and T'Last

    ---------------------------

    -- set-up image Header

    package F32_Header is new Image(MD_Tm, Float'Last);

    use BS70;
    F32_Image : F32_Header.Image_Rec := F32_Header.Metadata(MD_NAXISn,
                ((BZERO,    1*    "0.0"),
                 (BSCALE,   1*    "1.0"),
                 (BLANK,    1*    "255"),
                 (DATAMIN,  1*      "0"),
                 (DATAMAX,  1*    "255")));


    -- set-up transfer buffer for Write
   type Float_Arr is array (Positive_Count range <>) of Float;
   package MMDD_TTmm is new Numeric_Type(MD_Tm, Float_Arr, FLoat_Arr);


   package F32_Data is new FITS_IO.Data_Unit(MD_Tm);
   DUAccess : Init.Access_Rec;



    -- simulate some data

    Current_F32Column : F32_Data.T_Arr(1..ColLength);

    function Generate_Data(R : FITS_IO.Count; ColLength : FITS_IO.Positive_Count;
                        Undef_Valid : Boolean; Undef_Value : Float) return F32_Data.T_Arr
    is
        Col : F32_Data.Buffer(1 .. ColLength);
    begin
        for I in Col'Range loop Col(I) := Float(I)-1.0; end loop;
        if (Undef_Valid) then Col(1 + ((R-1) mod ColLength)) := Undef_Value; end if;
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


 Init.Init_Writes
         (DUType           => In_File_DUType,
            --BITPIX           => In_File_BITPIX,
         Undef_Phys_Used   => MD_Undef_Valid,
         Undef_Phys        => MD_Undef_Value,
         Undef_Raw_Valid   => MD_Undef_Valid,
         Undef_Raw         => -100.0,
         DU_Access         => DUAccess);

 Init.Put_Access_Rec(DUAccess);
 declare
    ArrKeys : Header.Valued_Key_Record_Arr := Init.To_Array_Keys(DUAccess);
 begin
    Init.Put_Array_Keys(ArrKeys);
 end;


 TIO.Put_Line("Writing: " & Temp_File_Name); 

 -- write Header

 Header.Write_Card_SIMPLE(Out_File, True);
 F32_Image.Target_BITPIX := DUAccess.BITPIX;
 F32_Header.Image_Rec'Output(Out_Stream, F32_Image);
 Header.Close(Out_File);

-- write Data Unit

 for I in 1 .. RowLength
 loop
     Current_F32Column := Generate_Data(I, ColLength,
                            MD_Undef_Valid, MD_Undef_Value);
     F32_Data.Write(Out_File, DUAccess, Current_F32Column);
 end loop;
 File.Misc.Write_Padding(Out_File, SIO.Index(Out_File), File.Misc.DataPadValue);

-- succesfully written, close and rename

 SIO.Close(Out_File);

 Ada.Directories.Rename(Temp_File_Name, File_Name);

 TIO.Put_Line("Ready  : " & File_Name); 

exception
  when Except_ID : others => TIO.Put_Line(TIO.Standard_Error, Exception_Information(Except_ID));
end create;

