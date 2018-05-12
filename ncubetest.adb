
with FITS.Data;
with FITS.Size;
use  FITS.Size;

with ncube;
with Ada.Text_IO;
use Ada.Text_IO;


procedure ncubetest
is
 Offset    :  FPositive := 12;
-- MaxCoords :  NCube.MyVector := (3,4,5,others => 1);
 MaxCoords :  NCube.Coord_Type := (3,4,5);--,others => 1);
-- Coords    :  NCube.MyVector := (9,9,9,others => 1);
 Coords    :  NCube.Coord_Type := (9,9,9);--,others => 1);
begin

-- NCube.To_Coords (Offset, MaxCoords, Coords);
-- Put(FPositive'Image(Offset) & ": ");
-- for I in Coords'Range
-- loop
--  Put(" " & FPositive'Image(Coords(I)));
-- end loop;

 -- Generic : do all Offsets

-- generic
--  type Item is (<>);
--  type Coll is array(FPositive range <>) of Item;
--  with function Value ( Coord : in Coord_Type ) return Item;
-- procedure Fill_In (DataVector : in out Coll; MaxCoords : in Coord_Type);-- is null;

 declare
 -- type Float32 is new FITS.Data.Float_32;
  -- FIXME instatiation fails with: Excpected discrete type. With Int is ok.
  type Int32 is new FITS.Data.Integer_32;

  type MyData is array (FPositive range <>) of Int32;

  function Int32Value ( Coord : in NCube.Coord_Type ) return Int32
  is 
  begin 
   return 2;
  end;

  procedure Fill_In_Int32 is new
   NCube.Fill_In (Int32, MyData, Int32Value);

  DInt32 : MyData(1..60);

 begin

  Fill_In_Int32(DInt32,MaxCoords);
  Put_Line("Data Arrary values: ");
  for I in DInt32'Range
  loop
   Put(" " & Int32'Image(DInt32(I)));
  end loop;

 end;



end ncubetest;



