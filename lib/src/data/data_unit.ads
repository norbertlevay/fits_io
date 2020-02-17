
-- Data Unit resides in FITS File
-- File access needed


with Ada.Streams.Stream_IO;

generic
  type T is private;
package Data_Unit is

 package SIO renames Ada.Streams.Stream_IO;
 use SIO;


-- raw data array in FITS file

generic
  with procedure Element (V : in T);
procedure Read_Array_Values
   (F : SIO.File_Type;
    Length : in Positive_Count;
    First  : in Positive := 1);
-- from current Block's First element read Length data elements
-- First = 1 and Length = DUSize will read all Data Unit
-- current Block is where SIO.File_Index points to



-- Conversion to physical values
-- Vphys = BZERO + BSCALE * Varr
 
-- converted values have physical meaning as BUNIT

 generic
  type Tout is private;
  with function "+" (L, R : in Tout) return Tout is <>;
  with function "*" (L, R : in Tout) return Tout is <>;
  with function "+" (R : in T) return Tout is <>;
 package Physical is


 -- BZERO BSCALE = any AND no BLANK
 generic
  with procedure Element_Value(V : in Tout);
  with function Is_Valid(V : in T) return Boolean is <>;
  with procedure Invalid is <>;
 procedure Read_Valid_Scaled_Values
        (F : SIO.File_Type; 
        Length : in Positive_Count;
        BZERO  : in Tout;
        BSCALE : in Tout;
        Undef_Val : in Tout;
        First  : in Positive := 1);

 -- BZERO BSCALE = any AND BLANK provided
 generic
  with procedure Element_Value(V : in Tout);
  with function Is_Valid(V : in T) return Boolean is <>;
  with procedure Invalid is null;
 procedure Read_Matched_Valid_Scaled_Values
        (F : SIO.File_Type;
        Length : in Positive_Count;
        BZERO  : in Tout;
        BSCALE : in Tout;
        BLANK  : in T;
        Undef_Val : in Tout;
        First  : in Positive := 1);




-- optimized variants for special cases

-- BZERO BSCALE = 0.0 1.0 AND BLANK ??
generic
  with procedure Element_Value(V : in Tout);
  with function Is_Valid(V : in T) return Boolean is <>;
  with procedure Invalid is <>;
 procedure Read_Valid_Values
        (F : SIO.File_Type; 
        Length : in Positive_Count;
        Undef_Val : in Tout;
        First  : in Positive := 1);


 -- BZERO BSCALE = Tab11 (BLANK don't care)
 generic
  with procedure Element_Value(V : in Tout);
  with procedure Undefined_Value is null;
  with procedure Invalid is <>;
  with function Is_Valid(V : in T) return Boolean is <>;
 procedure Read_Sign_Converted_Integers
        (F : SIO.File_Type;
        Length : in Positive_Count;
        Undef_Val : in Tout;
        First  : in Positive := 1);


end Physical;



end Data_Unit;




