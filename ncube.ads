
with FITS.Size; -- NAXISn Coordinate type needed
use  FITS.Size; -- NAXISn Coordinate type needed
with FITS.Data; -- Data_Arr needed

package ncube is

 type Coord_Type is new FITS.Size.Dims_Type;

 procedure To_Coords (Offset    : in  FPositive;
                      MaxCoords : in  Coord_Type;
                      Coords    : out Coord_Type);

-- -------------------------------------------------

-- FITS.Size.ads :
--   MaxAxes : constant Positive := 999; -- [FITS, Sect 4.4.1]
--   type Dims_Type is array (1..MaxAxes) of FPositive;

 -- Solution 1:
 -- write all data in one step - "small" data compared to computer memory

-- below compiles but see FIXME
-- generic
--  type Index is (<>);
--  type Item is (<>);-- OR: private; OR limited private;
--  with function DValue ( Coord : in Coord_Type ) return Item;
-- procedure DFill_In (Data_Acc :  access FITS.Data.Data_Arr);-- is null;
 -- FIXME above commented line works BUT
 --       constraint not allowed after .Data_Arr above: However we know that ValueType chould be DType
 --       use directly array !? instead of Data_Arr?
 -- implements [FITS Sect ??] ordering N-dimensional data by columns
 -- into an 1-dimensional array

 generic
  type Item is (<>);
  type Coll is array(FPositive range <>) of Item;
  with function Value ( Coord : in Coord_Type ) return Item;
 procedure Fill_In (DataVector : in out Coll; MaxCoords : in Coord_Type);-- is null;




 -- Solution 2:
 -- For "big" data: write by group of N-rows
 -- ...



end ncube;


