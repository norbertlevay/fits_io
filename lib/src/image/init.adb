
with Optional;

with Ada.Text_IO;

with Interfaces; use Interfaces; -- or use V3_Types

with Ada.Strings.Bounded;

package body Init is

   package TIO renames Ada.Text_IO;

   function BITPIX_To_DU_Type(BITPIX : Integer) return DU_Type
   is  
   begin
      case(BITPIX) is
         when  8 => return UInt8;
         when 16 => return Int16;
         when 32 => return Int32;
         when 64 => return Int64;
         when -32 => return F32;
         when -64 => return F64;
         when others => return Uint8; -- FIXME Error invalid BITPIX
      end case;
   end BITPIX_To_DU_Type;




   procedure DU_Type_To_BITPIX
      (DUType : DU_Type;
      BITPIX  : out Integer;
      Aui     : out Float)
   is
   begin
      -- implements Tab11 FITSv3
      case(DUType) is
         when   Int8 => Aui :=                -128.0; BITPIX :=   8; -- -2**7
         when  UInt8 => Aui :=                   0.0; BITPIX :=   8;
         when  Int16 => Aui :=                   0.0; BITPIX :=  16;
         when UInt16 => Aui :=               32768.0; BITPIX :=  16; --  2**15
         when  Int32 => Aui :=                   0.0; BITPIX :=  32;
         when UInt32 => Aui :=          2147483648.0; BITPIX :=  32; --  2**31
         when  Int64 => Aui :=                   0.0; BITPIX :=  64;
         when UInt64 => Aui := 9223372036854775808.0; BITPIX :=  64; --  2**63
         when    F32 => Aui :=                   0.0; BITPIX := -32;
         when    F64 => Aui :=                   0.0; BITPIX := -64;
      end case;
   end DU_Type_To_BITPIX;


   -- DU_Type & DATAMIN DATAMAX -> [A, B]

   procedure DU_Type_Min_Max(DUIntType : in DU_Int_Type; Min, Max : out Float)
   is
   begin
      case(DUIntType) is
         when  Int8  => Min := Float( Integer_8'First);  Max := Float( Integer_8'Last);
         when UInt8  => Min := Float(Unsigned_8'First);  Max := Float(Unsigned_8'Last);
         when  Int16 => Min := Float( Integer_16'First); Max := Float( Integer_16'Last);
         when UInt16 => Min := Float(Unsigned_16'First); Max := Float(Unsigned_16'Last);
         when  Int32 => Min := Float( Integer_32'First); Max := Float( Integer_32'Last);
         when UInt32 => Min := Float(Unsigned_32'First); Max := Float(Unsigned_32'Last);
         when  Int64 => Min := Float( Integer_64'First); Max := Float( Integer_64'Last);
         when UInt64 => Min := Float(Unsigned_64'First); Max := Float(Unsigned_64'Last);
         when F32 | F64 => null; -- FIXME DU_Int_Type but compiler complains missing F32 F64 ?!
      end case;
   end DU_Type_Min_Max;


   procedure Linear_Scale
      (DUIntType : in DU_Int_Type;
      DATAMIN, DATAMAX : in Float;
      A : out Float; B : out Float)
   is
      Min, Max : Float;
   begin

     if ((DATAMAX - DATAMIN) = 0.0)
     then
        null;-- Programming Error
     end if;

     DU_Type_Min_Max(DUIntType, Min, Max);

     B := (Max - Min) / (DATAMAX - DATAMIN);
     A := Max - B * DATAMAX;

   end Linear_Scale;


end Init;
