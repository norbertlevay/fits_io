--
-- Example convert FLoat32 -> Int16
--
-- FIXME: assume Primary HDU. What if other HDU is IMAGE type too?
--
-- demonstrate usage if data unit is "big":
-- all data will not fit into memory, needs to be processed
-- in chunks.


with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Traceback.Symbolic;
with Ada.Unchecked_Conversion;
with Ada.Streams.Stream_IO;
with Interfaces;

with Ada.Unchecked_Conversion;

with File;   use File;
with File_Funcs;  use File_Funcs;
with File.Misc;   use File.Misc;
with Keyword_Record; use Keyword_Record;
with Header; use Header;
with Mandatory; use Mandatory; -- NAXIS_Arr needed
with Optional;
with Optional.Reserved; use Optional.Reserved;


-- new Data interface
with V3_Types; use V3_Types;
with V3_Data_Unit; use V3_Data_Unit;
with Data_Funcs; use Data_Funcs;

with Data_Block;
with Data_Unit;
with NCube;
with Data_Value; use Data_Value;

procedure minmaxseq
is

 package TIO renames Ada.Text_IO;
 package SIO renames Ada.Streams.Stream_IO;
 use SIO;


    generic
        type T is private;
   procedure Revert_Bytes( Data : in out T );
   procedure Revert_Bytes( Data : in out T )
   is 
     Size_Bytes : Positive := T'Size / Interfaces.Unsigned_8'Size;
     type Arr4xU8 is array (1..Size_Bytes) of Interfaces.Unsigned_8;

     function Data_To_Arr is
       new Ada.Unchecked_Conversion(Source => T, Target => Arr4xU8);
     function Arr_To_Data is
       new Ada.Unchecked_Conversion(Source => Arr4xU8, Target => T);

     Arr  : Arr4xU8 := Data_To_Arr(Data);
     ArrO : Arr4xU8;
   begin

     for I in Arr'Range
     loop
       ArrO(I) := Arr(1 + Size_Bytes - I); 
     end loop;

     Data := Arr_To_Data(ArrO);

   end Revert_Bytes;




 InFile   : SIO.File_Type;
 HDUStart : SIO.Positive_Count := 1; -- Primary HDU only

 BITPIX  : Integer;
 NAXISn  : NAXIS_Arr(1..3);
 DUSize  : SIO.Positive_Count;
 DUStart : SIO.Positive_Count;

begin 

 if(Argument_Count /= 1 )
 then 
  Put_Line("Usage  " & Command_Name & " <file name>");
  return;
 else
   SIO.Open(InFile, SIO.In_File, (Argument(1)));
 end if;

 Set_File_Block_Index(InFile,HDUStart);

 declare
  HDUInfo : HDU_Info_Type := Read_Header(InFile);
 begin
  DUStart := File_Block_Index(InFile);
  DUSize  := SIO.Positive_Count(Data_Unit_Size_elems(HDUInfo.NAXISn));
  BITPIX := HDUInfo.BITPIX;
  NAXISn := HDUInfo.NAXISn;
 end;
 
 Put_Line("DU Start [blocks] :" & SIO.Positive_Count'Image(DUStart)); 
 Put_Line("DU Size  [element count]:" & SIO.Positive_Count'Image(DUSize)); 

 declare
   PlaneLength : constant SIO.Positive_Count := NAXISn(1)*NAXISn(2);
   type F32_Plane is array(SIO.Positive_Count range <>) of Float_32;
   procedure ReadRawPlane is new NCube.Read_Raw_Plane(Float_32, F32_Plane);
   procedure RevBytes is new Revert_Bytes(Float_32);
    F32Plane : F32_Plane(1..PlaneLength);
    Max : Float_32 := Float_32'First;
    Value : Float_32;
 begin
    for I in 1..NAXISn(3)
    loop
--        TIO.Put(SIO.Positive_Count'Image(I));
        ReadRawPlane(InFile,F32Plane);
        for I in F32Plane'Range
        loop
            RevBytes(F32Plane(I));
            if(F32Plane(I)'Valid)
            then
                if(F32Plane(I)>Max) then Max := F32Plane(I); end if; 
            end if;
        end loop;
        TIO.Put(Float_32'Image(Max));
    end loop;
end;

 SIO.Close(InFile);

 Put_Line("done.");


 exception

  when Except_ID : others =>
     declare
      Error : Ada.Text_IO.File_Type := Standard_Error;
     begin
      New_Line(Error);
      Put_Line(Error, "Exception_Information: ");
      Put_Line(Error, Exception_Information( Except_ID ) );
      New_Line(Error);
      Put_Line(Error, "Call stack traceback symbols: addr2line -e ./fits addr1 addr2 ...");
      Put_Line(" > Trace-back of call stack: " );
      Put_Line( GNAT.Traceback.Symbolic.Symbolic_Traceback(Except_ID) );
      -- See more at: http://compgroups.net/comp.lang.ada/gnat-symbolic-traceback-on-exceptions/1409155#sthash.lNdkTjq6.dpuf
      -- Do the same manually, use:
      -- addr2line -e ./fits addr1 addr2 ...
     end;
end minmaxseq;

