
with
    Ada.Text_IO,
    Ada.Direct_IO;


use
    Ada.Text_IO;

package body Commands is


 --
 -- print header to screen
 --

 procedure Read_HeaderFromTextFile( FileName : in String;
                               Header   : out HeaderBuffer;
                               CardsCount : out Natural;
                               Limit : in Positive := LimitDefault) is
   FileHandle     : File_Type;
   FitsCardBuffer : CardBuffer := (others => ' ');
   Count : Positive := 1;
 begin
   Open(FileHandle, In_File, FileName );
   loop
      exit when ( (FitsCardBuffer(1..3) = "END") or
                  (Count >= Limit) );

      FitsCardBuffer := To_CardBuffer(Ada.Text_IO.Get_Line( FileHandle ));
      Header(Count) := FitsCardBuffer;
      Count := Count + 1;
   -- Put_Line("Debug " & Integer'Image(Count));

   end loop;
   CardsCount := Count - 1;
   Close(FileHandle);
 end Read_HeaderFromTextFile;


 procedure Read_PrimaryHeader( FileName : in String;
                               Header   : out HeaderBuffer;
                               CardsCount : out Natural;
                               Limit : in Positive := LimitDefault) is
   FileHandle     : File_Type;
   FitsCardBuffer : CardBuffer := (others => ' ');
   Count : Positive := 1;
 begin
   Initialize;
   Open(FileHandle, In_File, FileName );
   loop
      exit when ( (FitsCardBuffer(1..3) = "END") or
                  (Count >= Limit) );
      FitsFile.Get( FileHandle, FitsCardBuffer );
      Header(Count) := FitsCardBuffer;
      Count := Count + 1;
   end loop;
   CardsCount := Count - 1;
   Close(FileHandle);
 end Read_PrimaryHeader;


 procedure Print_Header( Header : in HeaderBuffer ) is
   FitsCardBuffer : CardBuffer := (others => ' ');
   Count : Positive := 1;
 begin
   loop
      exit when ( (FitsCardBuffer(1..3) = "END") or
                  (Count >= LimitDefault) );
      FitsCardBuffer := Header(Count);
      Put_Line(FitsCardBuffer);
      Count := Count + 1;
   end loop;
 end Print_Header;


 procedure Print_PrimaryHeader( FileName : in String;
                                   Limit : in Positive := LimitDefault ) is
   EmptyCard : CardBuffer := ( others => ' ' );
   Header    : HeaderBuffer := ( others => EmptyCard );
   CardsCount : Natural;
 begin
   Read_PrimaryHeader( FileName, Header, CardsCount );
   Print_Header( Header );
 end Print_PrimaryHeader;


 --
 -- write header to file
 --

 procedure Write_PrimaryHeader( Header   : in HeaderBuffer;
 				FileName : in String ) is

 -- note Ada.Streams_IO allows direct access and also
 -- heterogenoius data to write to file
 -- Direct_IO assumes homogenius data in all file
 type my_rec is
  record
   mc : String(1..80) := (others => ' ');
  end record;
 package Ran_IO is new Ada.Direct_IO(my_rec);
   use Ran_IO;

 my_line : my_rec;
 FileHandle : Ran_IO.File_Type;
 i : Positive := 1;
 iRem : Positive;
 EmptyCard : CardBuffer := ( others => ' ' );

 begin
   Open(FileHandle, Out_File, FileName );

   loop
       exit when ( my_line.mc(1..3) = "END" );

       my_line.mc := Header(i);

       -- Write( FileHandle, my_line, myCnt );
       -- or omit myCnt and default is 'next record'
       Write( FileHandle, my_line );

       i := i + 1;

   end loop;

   -- fill up the block with empty cards

   -- Put_Line(Integer'Image(i));
   if (i-1) /= CardsInBlockCnt then
    iRem := ((i-1) mod CardsInBlockCnt ) + 1;
    for c in iRem .. CardsInBlockCnt loop

      -- Put_Line(Integer'Image(c));
      my_line.mc := EmptyCard;
      Write( FileHandle, my_line );

    end loop;
   end if;

   Close(FileHandle);
 end Write_PrimaryHeader;


 procedure Write_PrimaryHeader( HeaderFileName : in String;
 				   FileName   : in String ) is
   EmptyCard : CardBuffer   := ( others => ' ' );
   NewHeader : HeaderBuffer := ( others => EmptyCard );
   OldHeader : HeaderBuffer := ( others => EmptyCard );
   NewCardsCount : Natural;
   OldCardsCount : Natural;
 begin
   Read_HeaderFromTextFile( HeaderFileName, NewHeader, NewCardsCount );
   -- Print_Header(NewHeader);

   Read_PrimaryHeader( FileName, OldHeader, OldCardsCount );

   -- write only if same number of HeaderBlocks needed

   if (OldCardsCount / CardsInBlockCnt) = (NewCardsCount / CardsInBlockCnt)
   then
     Write_PrimaryHeader( NewHeader, FileName );
   else
     New_Line;
     Put_Line("Nothing written:");
     Put_Line("New header needs different number of blocks then the one in the file,");
     Put_Line("so it would require moving all data. NOT IMPLEMENTED YET.");
     Put_Line( "New Card Count  " & Integer'Image(NewCardsCount) & Integer'Image(NewHeader'Last) );
     Put_Line( "Old Card Count  " & Integer'Image(OldCardsCount) & Integer'Image(OldHeader'Last) );
     Put_Line( "Cards per Block " & Integer'Image(CardsInBlockCnt) );
     New_Line;
   end if;

 end Write_PrimaryHeader;

end Commands;

