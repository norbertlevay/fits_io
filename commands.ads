
with FitsFile;
use FitsFile;

package Commands is

 LimitDefault : constant Positive := 100*CardsInBlockCnt;
 type HeaderBuffer is array (1..LimitDefault) of CardBuffer;

 -- with HeaderBuffer

 procedure Read_PrimaryHeader( FileName : in  String;
                               Header   : out HeaderBuffer;
                               CardsCount : out Natural;
                               Limit : in Positive := LimitDefault );

 procedure Write_PrimaryHeader( Header   : in HeaderBuffer;
 				FileName : in String );

 procedure Print_Header( Header : in HeaderBuffer );

 -- directly with filenames

 procedure Print_PrimaryHeader( FileName : in String;
                                Limit : in Positive := LimitDefault );

 procedure Write_PrimaryHeader( HeaderFileName : in String;
 				FileName   : in String );

end Commands;

