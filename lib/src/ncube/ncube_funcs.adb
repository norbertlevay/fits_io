
with Mandatory; use Mandatory;-- NAXIS_Arr needed

package body NCube_Funcs is


 function To_Offset (Coords    : in  NAXIS_Arr;
                     MaxCoords : in  NAXIS_Arr)
   return Positive_Count
 is
  Offset : Positive_Count;
  Sizes  : NAXIS_Arr := MaxCoords;
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
    Accu  : Positive_Count := 1;
  begin
    for I in MaxCoords'First .. (MaxCoords'Last - 1)
    loop
     Accu := Accu * MaxCoords(I);
     Sizes(I) := Accu;
     -- FIXME Acc is not needed, init Sizes(1):=1 and use Sizes
    end loop;
  end;

  Offset := Coords(Coords'First);
  for I in (Coords'First + 1) .. Coords'Last
  loop
   Offset := Offset + (Coords(I) - 1) * Sizes(I - 1);
  end loop;

  return Offset;
 end To_Offset;




 procedure To_Coords (Offset    : in  Positive_Count;
                      MaxCoords : in  NAXIS_Arr;
                      Coords    : out NAXIS_Arr)
 is
    Sizes : NAXIS_Arr := MaxCoords;
    Divs :  NAXIS_Arr := MaxCoords;
    Rems :  NAXIS_Arr := MaxCoords;
    -- FIXME these inits are needed only to eliminate Ada error
    -- find other solution
 begin

  --
  -- generate size of each plane
  --
  declare
    Accu  : Positive_Count := 1;
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
    PrevRem : Count := Offset - 1;
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


