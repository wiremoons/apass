-------------------------------------------------------------------------------
-- BUILD FILE  : apass                                                       --
-- Description : A CLI password generator written in Ada.                    --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
-- use local packages
with Password_Manager;

procedure apass is
--------------------------
-- MAIN
--------------------------
begin

   Put_Line ("Program starts...");
   Put_Line ("Total words available: " & Password_Manager.Total_Words);
   Put_Line (Password_Manager.Basic_Password (3));

end apass;
