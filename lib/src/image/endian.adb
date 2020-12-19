
with Ada.Unchecked_Conversion;
with Interfaces;


package body Endian is

--  generic
--    type T is private;
--  procedure Revert_Bytes( Data : in out T );
  procedure Revert_Bytes( Data : in out T ) 
  is  
    Size_Bytes : Positive := T'Size / Interfaces.Unsigned_8'Size;
    type Arr4xU8 is array (1..Size_Bytes) of Interfaces.Unsigned_8;

    function Data_To_Arr is new Ada.Unchecked_Conversion(Source => T, Target => Arr4xU8);
    function Arr_To_Data is
      new Ada.Unchecked_Conversion(Source => Arr4xU8, Target => T); 

    Arr  : Arr4xU8 := Data_To_Arr(Data);
    ArrO : Arr4xU8;
  begin

    for I in Arr'Range
    loop
      ArrO(I) := Arr(1 + Size_Bytes - I); 
    end loop;

    Data := Arr_To_Data(ArrO);

  end Revert_Bytes;


--  generic
--    type T is private;
--    type T_Arr is array (Positive_Count range <>) of T;
--  procedure Check_And_Revert(Arr : in out T_Arr);
  procedure Check_And_Revert(Arr : in out T_Arr)
  is  
    procedure RevertBytes is new Revert_Bytes(T);
  begin
    -- FIXME add here check endianness: revert bytes only of needed
    -- FIXME build-time or run-time issue ?
    for I in Arr'Range
    loop
      RevertBytes(Arr(I));
    end loop;
  end Check_And_Revert;



end Endian;

