
with Mandatory; use Mandatory;-- NAXIS_Arr needed

package body NCube_Funcs is


 function To_DU_Index (Coords    : in  NAXIS_Arr;
                     MaxCoords : in  NAXIS_Arr)
   return Positive_Count
 is
  DUIndex : Positive_Count;
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

  DUIndex := Coords(Coords'First);
  for I in (Coords'First + 1) .. Coords'Last
  loop
   DUIndex := DUIndex + (Coords(I) - 1) * Sizes(I - 1);
  end loop;

  return DUIndex;
 end To_DU_Index;




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



  -- Plane and Volume Lnegths


  function Plane_Length
    (Plane : in NAXIS_Arr) return Positive_Count
  is
    PlaneLen : Positive_Count := 1;
  begin
    for I in Plane'Range
    loop
      PlaneLen := PlaneLen * Plane(I);
    end loop;
    return PlaneLen;
  end Plane_Length;


  function Volume_Length
    (First : in NAXIS_Arr;
    Last   : in NAXIS_Arr) return Positive_Count
  is
    L : Positive_Count := 1;
  begin

    -- FIXME sanity check that First & Last are equal length

    for I in First'Range
    loop
      L := L * (1 + Positive_Count(Last(I) - First(I)));
    end loop;
    return L;
  end Volume_Length;


end NCube_Funcs;


