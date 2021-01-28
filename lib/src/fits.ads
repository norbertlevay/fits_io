
with Ada.Strings.Bounded;
with Ada.Streams.Stream_IO;

package FITS is

   package SIO renames Ada.Streams.Stream_IO;

  type     Count          is new SIO.Count;
  subtype  Positive_Count is Count range 1 .. Count'Last;

  subtype NAXIS_Index is Integer range 1 .. 999;
  type    NAXIS_Array is array (NAXIS_Index range <>) of Positive_Count;

  subtype String_80 is String(1 .. 80);
  type String_80_Array is array (Positive_Count range <>) of String_80;

    ENDCard   : constant String_80 := ('E','N','D', others => ' ');
    EmptyCard : constant String_80 := (others => ' ');

    type DU_Type is
      (Int8, UInt16, UInt32, UInt64,
      UInt8,  Int16,  Int32,  Int64,
      F32, F64);

  package BS_8 is new Ada.Strings.Bounded.Generic_Bounded_Length( 8);
  package BS20 is new Ada.Strings.Bounded.Generic_Bounded_Length(20);
  package BS70 is new Ada.Strings.Bounded.Generic_Bounded_Length(70);

  type BS_8_Array  is array (Natural range <>) of BS_8.Bounded_String;

end FITS;
