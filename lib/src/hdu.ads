
-- FIXME Tcalc in Scaling should be generic (now it is Float)

with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Strings.Bounded;

with Cache; use Cache; -- Access_Rec & Cache_Rec
with DU_Pos; use DU_Pos; -- Pos_Rec

with FITS; use FITS;




package HDU is

   package SIO renames Ada.Streams.Stream_IO;

   type DU_End_Rec is
      record
         DU_Last   : FITS.Positive_Count; -- last position in DU
         Curr_Last : FITS.Count; -- last position Read/Written; 0 -> no access yet
      end record;


   type HDU_Type is record
      SIO_HDU_First : SIO.Positive_Count;
      Pos     : DU_Pos.Pos_Rec;
      Scaling : Access_Rec;
      Cache   : Cache_Rec;
      DU_End : DU_End_Rec;
   end record;

   subtype  Count is FITS.Count;
   subtype  Positive_Count is FITS.Positive_Count;

   subtype String_80 is String(1 .. 80);
   ENDCard   : constant String_80 := ('E','N','D', others => ' ');
   EmptyCard : constant String_80 := (others => ' ');

   package BS_8 is new Ada.Strings.Bounded.Generic_Bounded_Length( 8);
   package BS20 is new Ada.Strings.Bounded.Generic_Bounded_Length(20);
   package BS70 is new Ada.Strings.Bounded.Generic_Bounded_Length(70);

   subtype NAXIS_Index is FITS.NAXIS_Index;
   subtype NAXIS_Array is FITS.NAXIS_Array;

   subtype DU_Type is FITS.DU_Type;

   -- Image Array

   type Image_Rec(NAXIS : NAXIS_Index; Card_Count : Count) is
      record
         Data_Type   : DU_Type;
         NAXISn      : NAXIS_Array(1 .. NAXIS);
         Image_Cards : String_80_Array(1 .. Card_Count);
      end record;


   ----------------------
   -- Media Management --
   ----------------------

   procedure Reset
      (AHDU : in out HDU_Type;
      SIO_HDU_First : SIO.Positive_Count);

   procedure Write_Data_Unit_Padding(SIO_File : SIO.File_Type);

   function  End_Of_Data_Unit(AHDU : HDU_Type) return Boolean;
   function  Data_Unit_Size  (AHDU : HDU_Type) return Count;



   -------------------------
   -- Metadata Operations --
   -------------------------

   function  Read_Header
      (SIO_File : SIO.File_Type;
      AHDU : in out HDU_Type;
      Keys   : BS_8_Array)
      return Image_Rec;

      function  Read_Cards
         (SIO_File : SIO.File_Type;
         AHDU : in out HDU_Type;
         Keys   : BS_8_Array)
         return  String_80_Array;


         procedure Write_Header_Prim
            (SIO_File : SIO.File_Type;
            AHDU       : in out HDU_Type;
            Raw_Type    : DU_Type;
            NAXISn      : NAXIS_Array;
            Optional_Cards : String_80_Array);

         procedure Write_Header_Ext
            (SIO_File : SIO.File_Type;
            AHDU       : in out HDU_Type;
            Raw_Type    : DU_Type;
            NAXISn      : NAXIS_Array;
            Optional_Cards : String_80_Array);

         procedure Write_Cards
            (SIO_File : SIO.File_Type;
            AHDU       : in out HDU_Type;
            Cards : String_80_Array);


         -- set Scaling and Undefined Values in HDU_Type (but not in Header)

         procedure Set_Linear_Scaling    (AHDU : in out HDU_Type; A,B : Float);
         procedure Set_Undefined_Physical(AHDU : in out HDU_Type; Undef_Phys : Float);


         ---------------------------------
         -- Data Unit sequential access --
         ---------------------------------


         generic
         type T is private;
         type T_Arr is array (Positive_Count range <>) of T;
         with function "+"(V : in Float) return T     is <>; 
         with function "+"(V : in T)     return Float is <>; 
         with function Is_Undef  (V,U : in T) return Boolean is <>; 
         with function To_BITPIX (V   : in T) return Integer is <>; 
         procedure My_Read
            (SIO_File : SIO.File_Type;
            AHDU : in out HDU_Type;
            Item : out T_Arr;
            Last : out Count);


         generic
         type T is private;
         type T_Arr is array (Positive_Count range <>) of T;
         with function "+"(V : in Float) return T     is <>; 
         with function "+"(V : in T)     return Float is <>; 
         with function Is_Undef  (V,U : in T) return Boolean is <>; 
         with function To_BITPIX (V   : in T) return Integer is <>; 
         procedure My_Write
            (SIO_File : SIO.File_Type;
            AHDU : in out HDU_Type;
            Item : T_Arr);


         ----------------
         -- Exceptions --
         ----------------

         End_Error         : exception renames Ada.IO_Exceptions.End_Error;
         Programming_Error : exception;


         procedure Put_HDU_Type(AHDU : HDU_Type; Prefix : String := "");
         -- FIXME for debug only

      end HDU;

