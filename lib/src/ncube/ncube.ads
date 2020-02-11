

with Ada.Streams.Stream_IO;

--with Strict; use Strict; -- Positive_Arr needed, BUT uses FPositive
-- FIXME later unify Coord_Type and Positive_Arr
with NCube_Funcs; use NCube_Funcs; -- Coord_Type needed

package NCube is

 package SIO renames Ada.Streams.Stream_IO;

--type Positive_Arr is array (Positive range <>) of Positive;
-- FIXME later use the one from Strict.ads


 generic
  type T is private;
  type Tout is private;
  type Tout_Arr is array (Positive range <>) of Tout;
  with function Is_Valid(V : in T) return Boolean;
  with function "+" (L, R : in Tout) return Tout is <>; 
  with function "*" (L, R : in Tout) return Tout is <>; 
  with function "+" (R : in T) return Tout is <>; 
 procedure Read_Valid_Scaled_Line
                (F : SIO.File_Type; 
                BZERO  : in Tout;
                BSCALE : in Tout;
                Undef_Val : in Tout; 
		DUStart   : in Positive;
		MaxCoords : in Coord_Type;-- NAXOSn
                First  : in Coord_Type;
                Length : in Positive; -- may be at most NAXIS1
		Values : out Tout_Arr);




 generic
  type T is private;
  type Tout is private;
  type Tout_Arr is array (Positive range <>) of Tout;
  with function Is_Valid(V : in T) return Boolean;
  with function "+" (L, R : in Tout) return Tout is <>; 
  with function "*" (L, R : in Tout) return Tout is <>; 
  with function "+" (R : in T) return Tout is <>; 
 procedure Read_Valid_Scaled_Volume
                (File : SIO.File_Type; 
                BZERO  : in Tout;
                BSCALE : in Tout;
                Undef_Val : in Tout; 
		DUStart   : in Positive;
		MaxCoords : in Coord_Type;-- NAXISn
                First  : in Coord_Type;
                Last   : in Coord_Type;
		Volume : out Tout_Arr); -- result stored in 1D array, make it private later
					-- no override for indexing-operator in Ada



 


end NCube;
