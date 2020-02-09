
with Ada.Text_IO; use Ada.Text_IO;

procedure coord is

N : Integer := 3;

type Coord_Arr is array (Positive range 1 .. N) of Integer;

 function To_Offset (Coords    : in  Coord_Arr;
                     MaxCoords : in  Coord_Arr)
   return Positive
 is
  Offset : Positive;
  Sizes  : Coord_Arr := MaxCoords;
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
    Accu  : Positive := 1;
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



procedure print_coord(C : in Coord_Arr)
is
M : Coord_Arr := (10,10,10); 
begin
for I in C'Range loop
Put(Integer'Image(C(I)) & " ");
end loop;
Put(" : "& Integer'Image(To_Offset(C,M)));
New_Line;
end print_coord;



-- --------------------------------------------------------
-- coord generate algorithm variables
W : Integer := 1;

F :  Coord_Arr := (3,1,7);
L :  Coord_Arr := (5,1,9);

I : Integer := 1;
C :  Coord_Arr := F;
begin

C := F;
I := 1;
W := 1;

print_coord(C);

Outer_Loop:
loop

  loop

	if( C(W) = L(W) )
	then
	 C(W) := F(W);
	 W := W + 1;
  	 exit Outer_Loop when ( W > N );
	else
	 C(W) := C(W) + 1;
	 W := 1;
	 exit;
	end if;

  end loop;

  print_coord(C);

end loop Outer_Loop;

end coord;

