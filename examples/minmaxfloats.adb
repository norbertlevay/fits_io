--
-- Example convert FLoat32 -> Int16
--
-- FIXME: assume Primary HDU. What if other HDU is IMAGE type too?
--
-- demonstrate usage if data unit is "big":
-- all data will not fit into memory, needs to be processed
-- in chunks.


with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;
with Ada.Unchecked_Conversion;
with Ada.Streams.Stream_IO;
with Interfaces;

with Ada.Unchecked_Conversion;

with File;   use File;
with File_Funcs;  use File_Funcs;
with File.Misc;   use File.Misc;
with Keyword_Record; use Keyword_Record;
with Header; use Header;
with Mandatory; use Mandatory; -- NAXIS_Arr needed
with Optional;
with Optional.Reserved; use Optional.Reserved;


-- new Data interface
with V3_Types; use V3_Types;

with Image_Data;
--with Physical;
with Linear_Conv;


with V3_Privs_Image;

procedure minmaxfloats
is

  package TIO renames Ada.Text_IO;
  package SIO renames Ada.Streams.Stream_IO;
  use SIO;

  function Get_Int_16
    (Key : in String; 
    Cards : in Optional.Card_Arr) return Integer_16
  is
  begin
    for I in Cards'Range
    loop
      if(Cards(I)(1..Key'Length) = Key)
      then
        return Integer_16'Value(Cards(I)(11..30));
      end if;
    end loop;
    return 0;-- FIXME raise exception "Not_Found"
  end Get_Int_16;


  generic
  type T is digits <>;
  function Get_Float
    (Key : in String; 
    Cards : in Optional.Card_Arr;
    Default_Value : in T) return T;

    function Get_Float
      (Key : in String;
      Cards : in Optional.Card_Arr;
      Default_Value : in T) return T
    is
    begin
      for I in Cards'Range
      loop
        if(Cards(I)(1..Key'Length) = Key)
        then
          TIO.Put_Line(Cards(I)(11..30));
          return T'Value(Cards(I)(11..30));
        end if;
      end loop;
      return Default_Value;
    end Get_Float;

    function F64_Get_Float is new Get_Float(Float_64);
    function F32_Get_Float is new Get_Float(Float_32);



    InFile   : SIO.File_Type;
    HDUStart : SIO.Positive_Count := 1; -- Primary HDU only

    BITPIX  : Integer;
    NAXISn  : NAXIS_Arr(1..2);
    --NAXISn  : NAXIS_Arr(1..3);
    DUSize  : SIO.Positive_Count;
    DUStart : SIO.Positive_Count;

    PlaneLength :  SIO.Positive_Count;
    NPlanes     :  SIO.Positive_Count;


-- new

    type F64_Arr is array (SIO.Positive_Count range <>) of Float_64;
--    type F64_Arr_Acc is access F64_Arr;
    package F64_V3_Image is new V3_Privs_Image(Float_64,F64_Arr,Float_64);

    Max : Float_64 := Float_64'First;
    Invalid_Count : Natural := 0;

    procedure CB_PlaneData(Plane : in F64_Arr; PlaneCount : in SIO.Positive_Count)
    is
    begin
        TIO.Put(" " & SIO.Positive_Count'Image(PlaneCount));
        for I in Plane'Range
        loop
          if(Plane(I) /= F64NaN)
          then
            --TIO.Put(" " & Float_64'Image(Plane(I)));
            if(Plane(I)>Max) then Max := Plane(I); end if;
          else
            Invalid_Count := Invalid_Count + 1;
          end if;
        end loop;
    end CB_PlaneData;

    procedure Read_DU is new F64_V3_Image.Read_Data_Unit_By_Planes(F64NaN, CB_PlaneData);



  begin 

    if(Argument_Count /= 1 )
    then 
      Put_Line("Usage  " & Command_Name & " <file name>");
      return;
    else
      SIO.Open(InFile, SIO.In_File, (Argument(1)));
    end if;

    File.Set_File_Block_Index(InFile,HDUStart);

    --NPlanes     := NAXISn(3)*NAXISn(2);
    NPlanes     := 500;--NAXISn(2);
    PlaneLength := 500;--NAXISn(1);
    -- Size of Plane must be reasonable
    Put_Line("Plane Size [count]:" & SIO.Positive_Count'Image(PlaneLength)); 
    Put_Line("NPlanes           :" & SIO.Positive_Count'Image(NPlanes)); 

    declare
        Max : Float_64 := Float_64'First;
        Invalid_Count : Natural := 0;
        NAXISi : NAXIS_Arr(1..1) := (others => 500);--NAXISn(NAXISn'First..NAXISn'First);
    begin

        Read_DU(InFile, NAXISi);


      TIO.New_Line;
      TIO.Put_Line("Max Value :" & Float_64'Image(Max));
      TIO.Put_Line("Invalid Count: " & Natural'Image(Invalid_Count));
      TIO.Put_Line("Invalid Count [%]: " &
                Float_64'Image(100.0*Float_64(Invalid_Count)/Float_64(DUSize)));
    end;

    SIO.Close(InFile);
    Put_Line("done.");


  exception

    when Except_ID : others =>
      declare
        Error : Ada.Text_IO.File_Type := Standard_Error;
      begin
        New_Line(Error);
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
  end minmaxfloats;

