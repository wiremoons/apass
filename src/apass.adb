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
   Number_Of_Words   : constant Integer := 3;

begin

   --  print info on how to compile a 'release' version
   pragma Debug
     (Put_Line (Standard_Error, "DEBUG: build a 'release' version with: gprclean && gprbuild -XBUILD=release"));

   Put_Line ("Program 'apass' running...");
   Put_Line ("Total words available to construct passwords: " & Password_Manager.Total_Words);
   Put_Line ("Number of suggested passwords per group:" & Integer'Image (Total_Passwords));
   Put_Line ("Number of words to use per password string:" & Integer'Image (Number_Of_Words));
   New_Line (1);

   --  output of mixed lowercase with two punctuation marks and two random numbers surrounding the string
   while Total_Passwords > 0 loop
      Full_Password_Str := To_Unbounded_String (Password_Manager.Get_Random_Number);
      --Full_Password_Str := Full_Password_Str & To_Unbounded_String (Password_Manager.Get_Random_Mark);
      Append (Full_Password_Str, To_Unbounded_String (Password_Manager.Get_Random_Mark));
      Append (Full_Password_Str, To_Unbounded_String (Password_Manager.Basic_Password (Number_Of_Words)));
      Append (Full_Password_Str, To_Unbounded_String (Password_Manager.Get_Random_Mark));
      Append (Full_Password_Str, To_Unbounded_String (Password_Manager.Get_Random_Number));
      Password_Manager.Print_Password (To_String (Full_Password_Str));
      Total_Passwords := Total_Passwords - 1;
   end loop;
   Put_Line (" ");
   New_Line (1);
   
   Total_Passwords := 3;
   --  output of mixed lowercase with two punctuation marks and two random numbers surrounding the string
   while Total_Passwords > 0 loop
      Full_Password_Str := To_Unbounded_String (Password_Manager.Basic_Password (Number_Of_Words));
      Append (Full_Password_Str, To_Unbounded_String (Password_Manager.Get_Random_Mark));
      Append (Full_Password_Str, To_Unbounded_String (Password_Manager.Get_Random_Number));
      Password_Manager.Print_Password (To_String (Full_Password_Str));
      Total_Passwords := Total_Passwords - 1;
   end loop;
   Put_Line (" ");
   New_Line (1);
   
   Total_Passwords := 3;
   --  output of mixed lowercase with two punctuation marks and two random numbers surrounding the string
   while Total_Passwords > 0 loop
      Full_Password_Str := To_Unbounded_String (Password_Manager.Capitilised_Password (Number_Of_Words));
      Append (Full_Password_Str, To_Unbounded_String (Password_Manager.Get_Random_Mark));
      Append (Full_Password_Str, To_Unbounded_String (Password_Manager.Get_Random_Number));
      Password_Manager.Print_Password (To_String (Full_Password_Str));
      Total_Passwords := Total_Passwords - 1;
   end loop;
   Put_Line (" ");
   
end Apass;
