
with Ada.Text_IO; use Ada.Text_IO;

procedure coord is

N : Integer := 3;

type Coord_Arr is array (Positive range 1 .. N) of Integer;

procedure print_coord(C : in Coord_Arr) is
begin
for I in C'Range loop
Put(Integer'Image(C(I)) & " ");
end loop;
New_Line;
end print_coord;





W : Integer := 1;

F :  Coord_Arr := (3,1,5);
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

