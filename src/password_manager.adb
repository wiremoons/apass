-------------------------------------------------------------------------------
-- BUILD FILE  : apass                                                       --
-- Description : A CLI password generator written in Ada.                    --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------

with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNATCOLL.Terminal;       use GNATCOLL.Terminal;
with Ada.Strings.Fixed;
with Ada.Numerics.Discrete_Random;

package body Password_Manager is

   --  Add ability to obtain a random number within the range of the array length for Words_List_Array as spec source
   subtype Max_Words is Integer range 1 .. (Words_List_Array'Length);
   package Random_Word_Int is new Ada.Numerics.Discrete_Random (Max_Words);
   Gen_Word : Random_Word_Int.Generator;

   --  Add ability to generate a single random number between 0 and 99 named 'Random_Integer' when generated
   subtype Random_Num_Int is Integer range 0 .. 99;
   package Random_Integer is new Ada.Numerics.Discrete_Random (Random_Num_Int);
   Gen_Int : Random_Integer.Generator;

   --  Add ability to obtain a random number within the range of the array length for Marks_List_Array as spec source
   subtype Max_Marks is Integer range 1 .. (Marks_List_Array'Length);
   package Random_Marks_Int is new Ada.Numerics.Discrete_Random (Max_Marks);
   Gen_Mark : Random_Marks_Int.Generator;

   --  Create a handle to manage colour output where supported
   Screen : Terminal_Info;

   function Basic_Password (Number_Of_Words : Positive := 3) return String is
      ------------------------------------------------------------------------------------------------------------------
      --  Basic_Password generates a password from the number of words specified by value of function param           --
      --  Number_of_Words (default 3). Words are concatenated into a string that is returned.                         --
      ------------------------------------------------------------------------------------------------------------------

      Random_Number : Integer          := 0;
      Password_Str  : Unbounded_String := Null_Unbounded_String;

   begin

      pragma Debug (Put_Line (Standard_Error, "[DEBUG] Words to obtain: " & Integer'Image (Number_Of_Words)));
      Random_Word_Int.Reset (Gen => Gen_Word);

      for I in 1 .. Number_Of_Words loop
         Random_Number := Random_Word_Int.Random (Gen => Gen_Word);
         pragma Debug (Put (Standard_Error, "[DEBUG] Random number:" & Integer'Image (Random_Number)));
         pragma Debug (Put_Line (Standard_Error, " is word: " & Words_List_Array (Random_Number)));
         Append (Password_Str, Words_List_Array (Random_Number));
      end loop;

      return To_String (Password_Str);

   end Basic_Password;

   function Capitilised_Password (Number_Of_Words : Positive := 3) return String is
      ------------------------------------------------------------------------------------------------------------------
      --  Capitilised_Password generates a password from the number of words specified by value of function param     --
      --  Number_of_Words (default 3). Words are title cased and are concatenated into a string that is returned.     --
      ------------------------------------------------------------------------------------------------------------------

      Random_Number : Integer          := 0;
      Password_Str  : Unbounded_String := Null_Unbounded_String;

   begin

      pragma Debug (Put_Line (Standard_Error, "[DEBUG] Words to obtain: " & Integer'Image (Number_Of_Words)));
      Random_Word_Int.Reset (Gen => Gen_Word);

      for I in 1 .. Number_Of_Words loop
         Random_Number := Random_Word_Int.Random (Gen => Gen_Word);
         pragma Debug (Put (Standard_Error, "[DEBUG] Random number:" & Integer'Image (Random_Number)));
         pragma Debug (Put_Line (Standard_Error, " is word: " & Words_List_Array (Random_Number)));
         Append (Password_Str, Title_Case_String (Words_List_Array (Random_Number)));
      end loop;

      return To_String (Password_Str);

   end Capitilised_Password;

   function Title_Case_String (Input_Str : String) return String is
   ------------------------------------------------------------------------------------------------------------------
   --  Title_Case_String converts the first letter of the string provided to an upper case letter.                 --
   --  The converted string is returned. If the string is zero length it is just returned as is.                   --
   ------------------------------------------------------------------------------------------------------------------

   begin

      if Input_Str'Length = 0 then
         return Input_Str;
      else
         return To_Upper (Input_Str (Input_Str'First)) & To_Lower (Input_Str (Input_Str'First + 1 .. Input_Str'Last));
      end if;

   end Title_Case_String;

   procedure Print_Password (Final_Password : String) is
   ---------------------------------------------------------------------------------------------------------------------
   --  Print_Password outputs the provided string but displaying it in colour if supported.                           --
   --  Output of colours can also be controlled by command line flag (TODO). Whats colours via settings (TODO)        --
   ---------------------------------------------------------------------------------------------------------------------
   begin

      if Final_Password'Length = 0 then
         Put_Line (Standard_Error, "ERROR: Zero length password string - no output available.");
         return;
      end if;

      pragma Debug (Put_Line (Standard_Error, "[DEBUG] final password: " & Final_Password));

      --  reset screen outputs to known default - otherwise Windows outputs first displayed password
      --  with a bold white background - rest here fixes that.
      if Screen.Has_Colors then
         Set_Color
           (Self       => Screen,
            Term       => Standard_Output,
            Style      => Reset_All,
            Foreground => Reset,
            Background => Reset);
         pragma Debug (Put_Line (Standard_Error, "[DEBUG] Reset : 'Set_Color'"));
      end if;

      for C in Final_Password'Range loop

         if Is_Digit (Final_Password (C)) then
            -- ouput of numbers in colour
            --  pragma Debug (Put (Standard_Error, " [DEBUG] DIGIT: " & Final_Password (C)));
            if Screen.Has_Colors then
               Screen.Set_Fg (Color => Green, Term => Standard_Output);
               Put (Final_Password (C));
               Screen.Set_Fg (Color => Reset, Term => Standard_Output);
            else
               Put (Final_Password (C));
            end if;
         elsif Is_Punctuation_Connector (Final_Password (C)) or Is_Special (Final_Password (C)) then
            --  outut of punctuation characters including '_' pragma Debug (Put (Standard_Error, " [DEBUG] MARK: " &
            --  Final_Password (C)));
            if Screen.Has_Colors then
               Screen.Set_Fg (Color => Blue, Term => Standard_Output);
               Put (Final_Password (C));
               Screen.Set_Fg (Color => Reset, Term => Standard_Output);
            else
               Put (Final_Password (C));
            end if;
         elsif Is_Letter (Final_Password (C)) then
            --  output of any characters 'a .. z' or 'A .. Z' : NO COLOUR USED pragma Debug (Put (Standard_Error, "
            --  [DEBUG] LETTER: " & Final_Password (C)));
            Put (Final_Password (C));
         else
            --  output of anyting else not covered by the above specifics pragma Debug (Put (Standard_Error, " [DEBUG]
            --  OTHER: " & Final_Password (C)));
            if Screen.Has_Colors then
               Screen.Set_Fg (Color => Cyan, Term => Standard_Output);
               Put (Final_Password (C));
               Screen.Set_Fg (Color => Reset, Term => Standard_Output);
            else
               Put (Final_Password (C));
            end if;
         end if;
      end loop;

      --  flush all outputs issued with 'Put' only above
      --Put_Line (" ");
      Put ("   ");

      --  reset screen outputs to ensure back to normal
      if Screen.Has_Colors then
         Set_Color
           (Self       => Screen,
            Term       => Standard_Output,
            Style      => Reset_All,
            Foreground => Reset,
            Background => Reset);
         pragma Debug (Put_Line (Standard_Error, "[DEBUG] Reset : 'Set_Color'"));
      end if;

   end Print_Password;

   function Get_Random_Number return String is
   ---------------------------------------------------------------------------------------------------------------------
   --  Get_Random_Number returns a random number converted to a string. Selected from the 'Random_Num_Int' range.     --
   --  Leading Integer space trimmed from string before it is returned.                                               --
   --  If a single digit random number is generated a leading zero is added so it always returns 'length = 2          --
   ---------------------------------------------------------------------------------------------------------------------
      Tmp_Str : Unbounded_String := Null_Unbounded_String;

   begin

      Random_Integer.Reset (Gen => Gen_Int);

      Tmp_Str :=
        To_Unbounded_String (Ada.Strings.Fixed.Trim (Random_Integer.Random (Gen => Gen_Int)'Image, Ada.Strings.Left));

      pragma Debug (Put_Line (Standard_Error, "[DEBUG] Random integer: " & To_String (Tmp_Str)));

      if Length (Tmp_Str) /= 2 then
         Insert (Tmp_Str, 1, "0");
         return To_String (Tmp_Str);
      else
         return To_String (Tmp_Str);
      end if;

   end Get_Random_Number;

   function Get_Random_Mark return String is
   ---------------------------------------------------------------------------------------------------------------------
   --  Get_Random_Mark returns a random string (single character) selected from the 'Random_Marks_Int' range.         --
   --  The selected random string is obtained from the Marks_List_Array contained in spec file.                       --
   ---------------------------------------------------------------------------------------------------------------------
   begin

      Random_Marks_Int.Reset (Gen => Gen_Mark);
      return (Marks_List_Array (Random_Marks_Int.Random (Gen => Gen_Mark)));

   end Get_Random_Mark;

   function Total_Words return String is
   ---------------------------------------------------------------------------------------------------------------------
   --  Total_Words returns the total number of three letter words available in the 'Words_List_Array'.                --
   --  Leading Integer space trimmed from string before it is returned.                                               --
   ---------------------------------------------------------------------------------------------------------------------
   begin

      return (Ada.Strings.Fixed.Trim (Integer'Image (Words_List_Array'Length), Ada.Strings.Left));

   end Total_Words;

--  Setup the package by enabling screen handle
begin
   --  initialise screen outputs wanted for colour support
   Screen.Init_For_Stdout (Auto);
   Screen.Init_For_Stderr (Auto);
   --  show colour output capabilities in debug mode
   pragma Debug (Put_Line (Standard_Error, "[DEBUG] Checking for colour terminal support..."));
   pragma Debug (Put_Line (Standard_Error, "[DEBUG] Colour support: " & (if Screen.Has_Colors then "YES" else "NO")));
   pragma Debug
     (Put_Line (Standard_Error, "[DEBUG] ANSI support: " & (if Screen.Has_ANSI_Colors then "YES" else "NO")));
   Screen.Clear_To_End_Of_Line;

end Password_Manager;
