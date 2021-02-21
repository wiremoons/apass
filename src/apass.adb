-------------------------------------------------------------------------------
-- BUILD FILE  : apass                                                       --
-- Description : A CLI password generator written in Ada.                    --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
-- use local packages
with Password_Manager;

procedure Apass is
--------------------------
-- MAIN
--------------------------

   Full_Password_Str : Unbounded_String := Null_Unbounded_String;
   Total_Passwords   : Integer          := 3;

begin

   --  print info on how to compile a 'release' version
   pragma Debug
     (Put_Line (Standard_Error, "DEBUG: build a 'release' version with: gprclean && gprbuild -XBUILD=release"));

   Put_Line ("Program 'apass' running...");
   Put_Line ("Total words available: " & Password_Manager.Total_Words);
   New_Line (1);

   while Total_Passwords > 0 loop
      Full_Password_Str := To_Unbounded_String (Password_Manager.Get_Random_Number);
      Full_Password_Str := Full_Password_Str & To_Unbounded_String (Password_Manager.Get_Random_Mark);
      Full_Password_Str := Full_Password_Str & To_Unbounded_String (Password_Manager.Basic_Password (3));
      Full_Password_Str := Full_Password_Str & To_Unbounded_String (Password_Manager.Get_Random_Mark);
      Full_Password_Str := Full_Password_Str & To_Unbounded_String (Password_Manager.Get_Random_Number);
      Put_Line (To_String (Full_Password_Str));

      Total_Passwords := Total_Passwords - 1;

   end loop;


end Apass;
