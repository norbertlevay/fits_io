
with Ada.Text_IO;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with FITS_IO.Header;

package body FITS_IO.File is


  -- Instantiate Header for file-media

 function SIO_File_Next(File : Ada.Streams.Stream_IO.File_Type) return Card_Block  
  is 
   HBlk : Card_Block; 
  begin
   Card_Block'Read(Stream(File), HBlk); 
   return HBlk; 
  end SIO_File_Next; 
  pragma Inline(SIO_File_Next); 

  package SIO_File_Header is new Header(Source_Type =>  Ada.Streams.Stream_IO.File_Type, 
 	                                Index_Type  =>  Ada.Streams.Stream_IO.Positive_Count, 
        	                        Next        =>  SIO_File_Next, 
                	                Index       =>  Ada.Streams.Stream_IO.Index, 
                        	        Set_Index   =>  Ada.Streams.Stream_IO.Set_Index); 
  use SIO_File_Header;


  -- File management functions

   procedure Move_Index
             (FitsFile : in SIO.File_Type;
              ByCount  : in SIO.Positive_Count) is
   begin
     SIO.Set_Index(FitsFile, SIO.Index(FitsFile) + ByCount);
   end Move_Index;
   pragma Inline (Move_Index);



   procedure Set_Index(FitsFile : in SIO.File_Type;
                       HDUNum   : in Positive)
   is
    CurDUSize_bytes  : FITS_IO.Count;
    CurHDUNum : Positive := 1;

   begin

    SIO.Set_Index(FitsFile, 1);
    -- move to begining of the Primary HDU

    while CurHDUNum < HDUNum
    loop

     CurDUSize_bytes  := Read_DUSize_bytes(FitsFile);
     -- CurDUSize_bytes  := SIO_File_Header.Read_DUSize_bytes(FitsFile);

     Ada.Text_IO.Put_Line("NEWSet_Index: " & FITS_IO.Count'Image(CurDUSize_bytes));


     if CurDUSize_bytes /= 0
     then
       Move_Index(FitsFile, SIO.Positive_Count(CurDUSize_bytes));
     end if;

     CurHDUNum := CurHDUNum + 1;
    end loop;

   end Set_Index;

end FITS_IO.File;
