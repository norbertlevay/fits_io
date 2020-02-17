

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with Mandatory; use Mandatory; -- Positive_Arr needed

package NCube is

 package SIO renames Ada.Streams.Stream_IO;


 generic
  type T is private;
  type Tout is private;
  type Tout_Arr is array (Positive_Count range <>) of Tout;
  with function Is_Valid(V : in T) return Boolean;
  with function "+" (L, R : in Tout) return Tout is <>; 
  with function "*" (L, R : in Tout) return Tout is <>; 
  with function "+" (R : in T) return Tout is <>; 
 procedure Read_Valid_Scaled_Line
   (F : SIO.File_Type; 
    BZERO  : in Tout;
    BSCALE : in Tout;
    Undef_Val : in Tout; 
    DUStart   : in Positive_Count;
    MaxCoords : in Positive_Arr;-- NAXISn
    First  : in Positive_Arr;
    Length : in Positive_Count; -- may be at most NAXIS1
    Values : out Tout_Arr);




 generic
  type T is private;
  type Tout is private;
  type Tout_Arr is array (Positive_Count range <>) of Tout;
  with function Is_Valid(V : in T) return Boolean;
  with function "+" (L, R : in Tout) return Tout is <>; 
  with function "*" (L, R : in Tout) return Tout is <>; 
  with function "+" (R : in T) return Tout is <>; 
 procedure Read_Valid_Scaled_Volume
                (File : SIO.File_Type; 
                BZERO  : in Tout;
                BSCALE : in Tout;
                Undef_Val : in Tout; 
        DUStart   : in Positive_Count;
        MaxCoords : in Positive_Arr;-- NAXISn
                First  : in Positive_Arr;
                Last   : in Positive_Arr;
        Volume : out Tout_Arr); -- result stored in 1D array, make it private later
                    -- no override for indexing-operator in Ada



 


end NCube;
