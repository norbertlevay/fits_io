
with Mandatory; use Mandatory;-- Positive_Arr needed

package body NCube_Funcs is


 function To_Offset (Coords    : in  Positive_Arr;
                     MaxCoords : in  Positive_Arr)
   return FPositive
 is
  Offset : FPositive;
  Sizes  : Positive_Arr := MaxCoords;
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
                      MaxCoords : in  Positive_Arr;
                      Coords    : out Positive_Arr)
 is
    Sizes : Positive_Arr := MaxCoords;
    Divs :  Positive_Arr := MaxCoords;
    Rems :  Positive_Arr := MaxCoords;
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



end NCube_Funcs;


