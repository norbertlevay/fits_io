
with FITS.Data; -- Data_Arr needed

with Ada.Text_IO;
use  Ada.Text_IO;

package body ncube is

 function To_Offset (Coords    : in  Coord_Type;
                     MaxCoords : in  Coord_Type)
   return FPositive
 is
  Offset : FPositive;
  Sizes  : Coord_Type := MaxCoords;
 begin
  if Coords'Length /= MaxCoords'Length
  then
   null;
   -- raise exception <-- needed this if ?
   -- no, check only high level inputs, this is not direct API call
   -- assume if code corrct, it is corrct here
  end if;

  --
  -- generate size of each plane
  --
  declare
    Accu  : FPositive := 1;
  begin
    for I in MaxCoords'First .. (MaxCoords'Last - 1)
    loop
     Accu := Accu * MaxCoords(I);
     Sizes(I) := Accu;
     -- FIXME Acc is not needed, init Sizes(1):=1 and use Sizes
    end loop;
  end;

  Offset := Coords(1);
  for I in (Coords'First + 1) .. Coords'Last
  loop
   Offset := Offset + (Coords(I) - 1) * Sizes(I - 1);
  end loop;

  return Offset;
 end To_Offset;




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
  Coord : Coord_Type := MaxCoords;
  -- FIXME should be same length not necessary values
 begin
  -- implement [FITS Sect xx] data order FORTRAN-style
  for I in DataVector'Range
  loop
   To_Coords(I,MaxCoords,Coord);

  -- Debug:
  Put(FPositive'Image(I) & ": ");
  for I in Coord'Range
  loop
   Put(" " & FPositive'Image(Coord(I)));
  end loop;
  New_Line;
  -- Debug End

   DataVector(I) := Value(Coord);
  end loop;

 end Fill_In;


 -- for cutout
 function Next_Coord(Coord  : in out Coord_Type;
                     Offset : in Coord_Type;
                     Vol    : in Coord_Type)
   return Coord_Type
   is
    flag : array (FPositive range 1..Coord'Length) of Boolean
         := (1=>True, others => False);
    -- carry over flag
   begin

    -- assert Coord'Range = Offest'Range = Vol'Range = 1 .. Coord'Last
    -- assert Offset + Vol < DataUnit in every coordinate

    for I in 2 .. Coord'Last
    loop

      if (flag (I-1)) then

        Coord(I) := Coord(I) + 1;
        if( Coord(I) > (Offset(I) + Vol(I) - 1) ) -- coordinate overflow
        then
          -- reset coordinate and carry-over
          Coord(I) := Offset(I);
          flag(I)  := True;
        end if;

      end if;

    end loop;

    return Coord;
   end Next_Coord;


end ncube;


