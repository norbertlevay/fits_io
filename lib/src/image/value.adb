
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Streams; use Ada.Streams;
with System;

with Numeric_Type;
with Endian;

-- FIXME stack/local arrays: Af Fin_Arr Fout_Arr Af, use 3x more space: price to pay
-- for saving if(Use_Undef AND Is_Undef()) at cycle twice

with Cache; -- Access_Rec


package body Value is

package Phys renames Physical;


procedure Raw_To_Phys
    (Raw_Arr : in Raw.Numeric_Arr;
    Scaling : Access_Rec;
    Phys_Arr : out Physical.Numeric_Arr)
is
 Af         : Raw.Numeric_Arr(Phys_Arr'Range);
 Fin_Arr    : Raw.Float_Arr(Phys_Arr'Range);
 Fout_Arr   : Phys.Float_Arr(Phys_Arr'Range);
   procedure Raw_CheckAndRevert is new Endian.Check_And_Revert(Raw.Numeric,Raw.Numeric_Arr);
   A : Float := Scaling.A;
   B : Float := Scaling.B;
begin

   -- BEGIN from HDU

   if(Scaling.Undef_Used)
   then
      Raw.Set_Undefined (Raw."+"(Scaling.Undef_Raw));
      Phys.Set_Undefined(Phys."+"(Scaling.Undef_Phys));
   end if;

   -- END from HDU



   Af := Raw_Arr;
   -- FIXME raises excpetion is Raw_Arr'Range /= Phys_Arr_Range

    -- low-level read

--    Raw.Numeric_Arr'Read(S, Af);
    Raw_CheckAndRevert(Af);

    -- conversions

    Fin_Arr := Raw.To_Float(Af);
    for I in Fin_Arr'Range
    loop
        Fout_Arr(I) := A + B * Fin_Arr(I);
    end loop;

    -- calc target-domain's Undef Value
    if(Raw.Is_Undefined_Valid)
    then
        if(Not Phys.Is_Undefined_Valid)
        then
            Phys.Set_Undefined(Phys."+"(A + B * Raw."+"(Raw.Get_Undefined)));
        end if;
    end if;

    Phys_Arr := Phys.To_Numeric(Fout_Arr);

end Raw_To_Phys;



procedure Read
    (S : not null access Ada.Streams.Root_Stream_Type'Class;
    A,B : in Float;
    Phys_Arr : out Physical.Numeric_Arr)
is
   Af : Raw.Numeric_Arr(Phys_Arr'Range);
   procedure Raw_CheckAndRevert is new Endian.Check_And_Revert(Raw.Numeric,Raw.Numeric_Arr);

   Dummy : Raw.Numeric;
   NBits : Integer := Af'Length * abs Raw.To_BITPIX(Dummy);
   -- FIXME how to find Object_Size of Raw.Numeric_Arr which is generic here ?
   -- GEM39: Item <-> Af and Buffer <-> Raw.Numeric_Arr
   -- Item_Size : constant Stream_Element_Offset := Buffer'Object_Size / Stream_Element'Size;
   Raw_Arr_Size : constant Stream_Element_Offset :=
                                 Ada.Streams.Stream_Element_Offset(NBits) / Stream_Element'Size;
   type SEA_Pointer is access all Stream_Element_Array (1 .. Raw_Arr_Size);
   function As_SEA_Pointer is new Ada.Unchecked_Conversion (System.Address, SEA_Pointer);
   Length : Ada.Streams.Stream_Element_Offset;
begin
   --Raw.Numeric_Arr'Read(S, Af);
   Ada.Streams.Read(S.all, As_SEA_Pointer(Af'Address).all, Length );
   --Ada.Streams.Stream_Element_Array'Read(S, As_SEA_Pointer(Af'Address).all );
   Raw_CheckAndRevert(Af);
   -- low level read done
   --Raw_To_Phys(Af, A,B, Phys_Arr);
end Read;



procedure Phys_To_Raw
    (Raw_Arr : out Raw.Numeric_Arr;
    Scaling : Access_Rec;
    Phys_Arr : in Physical.Numeric_Arr)
is
 Af         : Raw.Numeric_Arr(Phys_Arr'Range);
 Fin_Arr    : Phys.Float_Arr(Phys_Arr'Range);
 Fout_Arr   : Raw.Float_Arr(Phys_Arr'Range);
   procedure Raw_CheckAndRevert is new Endian.Check_And_Revert(Raw.Numeric,Raw.Numeric_Arr);
 A : Float := Scaling.A;
 B : Float := Scaling.B;
begin

   -- BEGIN from HDU

   if(Scaling.Undef_Used)
   then
      Raw.Set_Undefined (Raw."+"(Scaling.Undef_Raw));
      Phys.Set_Undefined(Phys."+"(Scaling.Undef_Phys));
   end if;

   -- END from HDU


    -- conversions

    Fin_Arr := Phys.To_Float(Phys_Arr);
    for I in Fin_Arr'Range
    loop
        Fout_Arr(I) := A + B * Fin_Arr(I);
    end loop;

    -- calc target-domain's Undef Value
    if(Phys.Is_Undefined_Valid)
    then
        if(Not Raw.Is_Undefined_Valid)
        then
            Raw.Set_Undefined(Raw."+"( (Phys."+"(Phys.Get_Undefined) - A) / B) );
        end if;
    end if;

    Af := Raw.To_Numeric(Fout_Arr);

    -- low-level write

    Raw_CheckAndRevert(Af);
--    Raw.Numeric_Arr'Write(S, Af);
   Raw_Arr := Af;

end Phys_To_Raw;



procedure Write
    (S : not null access Ada.Streams.Root_Stream_Type'Class;
    A,B : in Float;
    Phys_Arr : in Physical.Numeric_Arr)
is
   Af : Raw.Numeric_Arr(Phys_Arr'Range);
   procedure Raw_CheckAndRevert is new Endian.Check_And_Revert(Raw.Numeric,Raw.Numeric_Arr);

   Dummy : Raw.Numeric;
   NBits : Integer := Af'Length * abs Raw.To_BITPIX(Dummy);
   -- FIXME how to find Object_Size of Raw.Numeric_Arr which is generic here ?
   -- GEM39:
   Raw_Arr_Size : constant Stream_Element_Offset :=
                                 Ada.Streams.Stream_Element_Offset(NBits) / Stream_Element'Size;
   type SEA_Pointer is access all Stream_Element_Array (1 .. Raw_Arr_Size);
   function As_SEA_Pointer is new Ada.Unchecked_Conversion (System.Address, SEA_Pointer);

begin
   Put(Integer'Image(NBits));
   --Phys_To_Raw(Af, A,B, Phys_Arr);
   -- low level write
   Raw_CheckAndRevert(Af);
   Ada.Streams.Write(S.all, As_SEA_Pointer(Af'Address).all );
   --Ada.Streams.Stream_Element_Array'Write(S, As_SEA_Pointer(Af'Address).all );
--   Raw.Numeric_Arr'Write(S, Af);
end Write;



end Value;

