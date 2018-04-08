
with FITS.Size; -- NAXISn Coordinate type needed
with FITS.Data; -- Data_Arr needed

package body ncube is

-- generic
--  type Index is (<>);
--  type Item is (<>);
--  type Coll is array(Index range <>) of Item;
--  with function Value ( Coord : in Coord_Type ) return Item;
 procedure Fill_In (DataVector : in out Coll)
 is
 begin
  null;
  -- implement [FITS Sect xx] data order FORTRAN-style
 end Fill_In;

end ncube;


