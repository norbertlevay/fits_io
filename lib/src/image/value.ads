
with Ada.Streams;
with Numeric_Type;
with Cache; use Cache;-- Access_Rec

generic
 with package Raw      is new Numeric_Type(<>);
 with package Physical is new Numeric_Type(<>);
package Value is

   procedure Raw_To_Phys
    (Raw_Arr : in Raw.Numeric_Arr;
    Scaling : Access_Rec;
    Phys_Arr : out Physical.Numeric_Arr);

   procedure Phys_To_Raw
    (Raw_Arr : out Raw.Numeric_Arr;
    Scaling : Access_Rec;
    Phys_Arr : in Physical.Numeric_Arr);

end Value;

