
with FITS.Size; -- NAXISn Coordinate type needed
with FITS.Data; -- Data_Arr needed

with Ada.Text_IO;
use  Ada.Text_IO;

package body ncube is

-- type MyVector is array (Positive range <>) of Positive;

 procedure To_Coords (Offset    : in  Positive;
                      MaxCoords : in  MyVector;
                      Coords    : out MyVector)
 is
    Sizes : MyVector := MaxCoords;
    Divs :  MyVector := MaxCoords;
    Rems :  MyVector := MaxCoords;
    -- FIXME these inits are needed only to eliminate Ada error
    -- find other solution
 begin

  --
  -- generate size of each plane
  --
  declare
    Accu  : Positive := 1;
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
    PrevRem : Positive := Offset;
  begin
    for I in reverse MaxCoords'First .. MaxCoords'Last
    loop
      Divs(I) := 1 + PrevRem  /  Sizes(I);
      Rems(I) := PrevRem rem Sizes(I);
      -- FIXME rem gives 0 for multiples
      PrevRem := Rems(I);
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
 procedure Fill_In (DataVector : in out Coll)
 is
  Ita : Item;
 begin
  -- implement [FITS Sect xx] data order FORTRAN-style
  for I in DataVector'Range
  loop
   DataVector(I) := Ita;
  end loop;

 end Fill_In;

end ncube;


