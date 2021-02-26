-------------------------------------------------------------------------------
-- BUILD FILE  : apass                                                       --
-- Description : A CLI password generator written in Ada.                    --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line;      use Ada.Command_Line;
-- use local packages
with Password_Manager;
with Cmd_Flags;

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
     (Put_Line (Standard_Error, "[DEBUG] build a 'release' version with: gprclean && gprbuild -XBUILD=release"));

   --  Check if any command line options were used and if they are they will be executed 
   --  and then the program can exit here
   if Cmd_Flags.Command_Line_Flags_Exist then
      Set_Exit_Status (Success);
      pragma Debug (Put_Line (Standard_Error, "[DEBUG] exit with 'success'."));
      return; -- exit as flags found and executed

   else
      --  Set_Exit_Status (Failure); -- failed as no database found
      --  pragma Debug (Put_Line (Standard_Error, "[DEBUG] exit with 'failure'."));

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

   end if;

   Set_Exit_Status (Success);
   pragma Debug (Put_Line (Standard_Error, "[DEBUG] exit with 'success'."));
   return; -- exit as flags found and executed

end Apass;
