
with FitsFile;
use  FitsFile;

package Commands is

 procedure Print_Header( FileName : in String;
                         HDU_Num  : Positive := 1 );
 -- read Header from FileName and print to stdout

 procedure Write_Header( FitsFileName   : in String;
 			 HeaderFileName : in String;
                         HDU_Num        : Positive := 1 );
 -- write Header from HeaderFileName into FitsFileName
 -- HeaderFileName is text file, with one card per line
 -- and last line must be "END"

end Commands;

