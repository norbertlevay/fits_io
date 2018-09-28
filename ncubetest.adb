
with FITS; use FITS;

with ncube;
with Ada.Text_IO; use Ada.Text_IO;

-- generic
--  type Item is (<>);
--  type Coll is array(FPositive range <>) of Item;
--  with function Value ( Coord : in Coord_Type ) return Item;
-- procedure Fill_In (DataVector : in out Coll; MaxCoords : in Coord_Type);-- is null;


procedure ncubetest
is
 -- type Float32 is new FITS.Data.Float_32;
  -- FIXME Float32 instatiation fails with: Excpected discrete type.
  type Int32 is new FITS.Integer_32;
--  type Int32 is new FITS.Float_32;

  type MyData is array (FPositive range <>) of Int32;

  function Int32Value ( Coord : in NCube.Coord_Type ) return Int32
  is 
  begin 
   return 2;
  end;

  procedure Fill_In_Int32 is new
   NCube.Fill_In (Int32, MyData, Int32Value);

  MaxCoords  : NCube.Coord_Type := (3,4,5);
  DInt32_Acc : access MyData;

begin

  -- FIXME 60 = 3*4*5
  DInt32_Acc := new MyData(1..60);
  -- memory will be released at 'end'

  Fill_In_Int32(DInt32_Acc.all, MaxCoords);

  Put_Line("Data Arrary values: ");
  for I in DInt32_Acc.all'Range
  loop
   Put(" " & Int32'Image(DInt32_Acc.all(I)));
  end loop;

end ncubetest;



