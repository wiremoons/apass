-------------------------------------------------------------------------------
-- BUILD FILE  : apass                                                       --
-- Description : A CLI password generator written in Ada.                    --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
-- use local packages
with Password_Manager;

procedure Apass is
--------------------------
-- MAIN
--------------------------
begin

   --  print info on how to compile a 'release' version
   pragma Debug
      (Put_Line
          (Standard_Error,
           "DEBUG: build a 'release' version with: gprclean && gprbuild -XBUILD=release"));

   Put_Line ("Program 'apass' running...");
   Put_Line ("Total words available: " & Password_Manager.Total_Words);
   Put_Line (Password_Manager.Basic_Password (3));

end Apass;
