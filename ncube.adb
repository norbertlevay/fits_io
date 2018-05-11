
with FITS.Size; -- NAXISn Coordinate type needed
with FITS.Data; -- Data_Arr needed

with Ada.Text_IO;
use  Ada.Text_IO;

package body ncube is


 procedure To_Coords (Offset    : in  FPositive;
                      MaxCoords : in  Coord_Type;
                      Coords    : out Coord_Type)
 is
    Sizes : Coord_Type := MaxCoords;
    Divs :  Coord_Type := MaxCoords;
    Rems :  Coord_Type := MaxCoords;
    -- FIXME these inits are needed only to eliminate Ada error
    -- find other solution
 begin

  --
  -- generate size of each plane
  --
  declare
    Accu  : FPositive := 1;
  begin
    for I in MaxCoords'Range
    loop
     Accu := Accu * MaxCoords(I);
     Sizes(I) := Accu;
     -- FIXME Acc is not needed, init Sizes(1):=1 and use Sizes
    end loop;
  end;

  --
  -- calc divisions and fractions
  --
  declare
    PrevRem : FNatural := Offset - 1;
  begin
    for I in reverse MaxCoords'First .. MaxCoords'Last
    loop
      Divs(I) := 1 + PrevRem  /  Sizes(I);
      Rems(I) := 1 + PrevRem rem Sizes(I);
      -- FIXME rem gives 0 for multiples
      PrevRem := Rems(I) - 1;
    end loop;
  end;

  --
  -- pick the coordinates from Divs & Rems
  --
  Coords := Rems(Rems'First) & Divs(Rems'First..Divs'Last-1);
 end To_Coords;


-- generic
--  type Index is (<>);
--  type Item is (<>);
--  type Coll is array(Index range <>) of Item;
--  with function Value ( Coord : in Coord_Type ) return Item;
 procedure Fill_In (DataVector : in out Coll; MaxCoords : in Coord_Type)
 is
  Coord : Coord_Type;
 begin
  -- implement [FITS Sect xx] data order FORTRAN-style
  for I in DataVector'Range
  loop
   To_Coords(I,MaxCoords,Coord);
   DataVector(I) := Value(Coord);
  end loop;

 end Fill_In;

end ncube;


