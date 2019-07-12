--
-- FITS File contains a set of HDU's. This module provides operations with HDUs,
-- within one file or betwen two files.
--
-- (Operations _within_ HDU like get/set properties, Read/Write data, are in HDU module.)
--

with Ada.Streams.Stream_IO;

package FITSlib.File is

   package SIO renames Ada.Streams.Stream_IO;



   procedure Set_HDU
	   (File   : SIO.File_Type; 
	    HDUNum : Positive);


   -- and also something like:

   procedure Remove_HDU
	   (File   : SIO.File_Type; 
	    HDUNum : Positive) is null;

   procedure Copy_HDU
	   (FromFile   : SIO.File_Type; 
	    FromHDUNum : Positive;
	    ToFile     : SIO.File_Type; 
	    ToHDUNum   : Positive) is null;


end FITSlib.File;
