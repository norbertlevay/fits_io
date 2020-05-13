
-- Sequential Access:
-- if T_Arr'Length = NAXISi*NAXISi-1*...*NAXIS1
-- then repeating Read (NAXISn*NAXISn-1*...NAXISi+1)-times 
-- reads all DU sequentially
-- NOTE position to DUStart before 1st call in Read/Write_x_Data


-- NOTE we separate to Int and Float because Scale_Float checks 
-- that value is valid and Scale used fpr Int does not check validity.
-- Because in case of (U)Int, BLANK is in range of valid values
-- whereas for Floats NaN is invalid in Ada, woudl raise exception
-- for (U)Int:  new-BLANK = A + B * BLANK
-- for Float:   A + B * NaN -> raises exception

with Ada.Text_IO;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;
with Interfaces;

with Mandatory;     use Mandatory; -- NAXIS_Arr needed
with Keyword_Record; use Keyword_Record; -- FIndex needed in NAXIS_Arr
with Raw_Funcs; use Raw_Funcs;
with Raw;


package body Physical_Private is

  use SIO;

  package TIO renames Ada.Text_IO;



 package body Input is

  procedure Read_Array
    (F : SIO.File_Type;
    Data  : out Tm_Arr)
  is
    type Tf_Arr is array (Positive_Count range <>) of Tf;
    RawData : Tf_Arr(Data'First .. Data'Last);
    package Tf_Raw is new Raw(Tf,Tf_Arr);
  begin
    Tf_Raw.Read_Array(F, RawData);
    for I in RawData'Range
    loop
      Data(I) := Linear(RawData(I));
    end loop;
  end Read_Array;



-- generic
--  with procedure Data(Block : in Tm_Data_Block);
 procedure Read_All_Data_Unit
  (File : SIO.File_Type;
  NAXISn : in NAXIS_Arr)
 is
    type Tf_Arr is array (Positive_Count range <>) of Tf;
    package Tf_Raw is new Raw(Tf,Tf_Arr);

    procedure RawData(Block : in Tf_Raw.T_Data_Block)
    is
        ConvBlock : Tm_Data_Block;
    begin
        for I in Block'Range
        loop
            ConvBlock(I) := Linear(Block(I));
        end loop;
        Data(ConvBlock);
    end RawData;

    procedure Read_DU is new Tf_Raw.Read_All_Data_Unit(RawData);
 begin
     Read_DU(File, NAXISn);
 end Read_All_Data_Unit;










  procedure Read_Volume
    (File : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Last    : in NAXIS_Arr;
    Volume  : out Tm_Arr)
  is
    VolLength : Positive_Count := Raw_Funcs.Volume_Length(First, Last);

    type Tf_Arr is array (Positive_Count range <>) of Tf;
    RawVol: Tf_Arr(1 .. VolLength);

    package Tf_Raw is new Raw(Tf,Tf_Arr);

  begin

    Tf_Raw.Read_Volume(File, DUStart, NAXISn, First, Last, RawVol);

    for I in RawVol'Range
    loop
      Volume(I) := Linear(RawVol(I));
    end loop;

  end Read_Volume;

 end Input;



 package body Output is


  procedure Write_Array
    (F : SIO.File_Type;
    Data : in Tm_Arr)
  is
    type Tf_Arr is array (Positive_Count range <>) of Tf;
    RawData : Tf_Arr(Data'First .. Data'Last);
    package Tf_Raw is new Raw(Tf,Tf_Arr);
  begin
    for I in Data'Range
    loop
      RawData(I) := Linear(Data(I));
    end loop;
    Tf_Raw.Write_Array(F, RawData);
  end Write_Array;

 end Output;


end Physical_Private;

