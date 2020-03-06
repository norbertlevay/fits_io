
 with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;


package Unit is

package SIO renames Ada.Streams.Stream_IO;


-- raw data

generic
  type T is private;
  with procedure Element (V : in T);
procedure Read_Array_Values
   (F : SIO.File_Type;
    Length : in Positive_Count;
    First  : in Positive := 1);


-- physical data

generic
    type Tf is private;
    type Tm is private;
    type Tc is digits <>;
    with procedure Element_Value(V : in Tm);
    with function "+"(R : in Tc) return Tm is <>;
    with function "+"(R : in Tf) return Tc is <>;
 procedure Read_Values
        (F : SIO.File_Type;
        BZERO  : in Tc;
        BSCALE : in Tc;
        Length : in Positive_Count;
        First  : in Positive := 1);




generic
    type Tf is digits <>;
    type Tm is private;
    type Tc is digits <>;
    with procedure Element_Value(V : in Tm);
    with procedure Invalid(Index : in Positive_Count) is <>;
    with function "+"(R : in Tc) return Tm is <>;
    with function "+"(R : in Tf) return Tc is <>;
 procedure Read_Float_Values
        (F : SIO.File_Type;
        BZERO  : in Tc;
        BSCALE : in Tc;
        Undef  : in Tm;
        Length : in Positive_Count;
        First  : in Positive := 1);

end Unit;

