

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with Mandatory; use Mandatory; -- NAXIS_Arr needed

package NCube is

 package SIO renames Ada.Streams.Stream_IO;

 generic
  type T is private;
  type T_Arr is array (Positive_Count range <>) of T;
 procedure Read_Raw_Line
   (F : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Values  : out T_Arr);

 generic
  type T is private;
  type T_Arr is array (Positive_Count range <>) of T;
 procedure Read_Raw_Volume
   (File : SIO.File_Type;
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Last    : in NAXIS_Arr;
    Volume  : out T_Arr); -- FIXME  later make T_Arr private





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
    DUStart: in Positive_Count;
    NAXISn : in NAXIS_Arr;
    First  : in NAXIS_Arr;
    Length : in Positive_Count; --  at most NAXIS1
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
    DUStart : in Positive_Count;
    NAXISn  : in NAXIS_Arr;
    First   : in NAXIS_Arr;
    Last    : in NAXIS_Arr;
    Volume  : out Tout_Arr); -- FIXME result stored in 1D array, make it private later
                    -- no override for indexing-operator in Ada

end NCube;

