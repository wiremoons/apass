-------------------------------------------------------------------------------
-- BUILD FILE  : apass                                                       --
-- Description : A CLI password generator written in Ada.                    --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
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
         pragma Debug
           (Put_Line
              (Standard_Error,
               "[DEBUG] Random number:" &
               Integer'Image (Random_Number) &
               " is word: " &
               Words_List_Array (Random_Number)));
         Password_Str := Password_Str & Words_List_Array (Random_Number);
      end loop;

      return To_String (Password_Str);

   end Basic_Password;

   function Get_Random_Number return String is
   ---------------------------------------------------------------------------------------------------------------------
   --  Get_Random_Number returns a random number converted to a string. Selected from the 'Random_Num_Int' range.     --
   --  Leading Integer space trimmed from string before it is returned.                                               --
   ---------------------------------------------------------------------------------------------------------------------
   begin

      Random_Integer.Reset (Gen => Gen_Int);
      return (Ada.Strings.Fixed.Trim (Random_Integer.Random (Gen => Gen_Int)'Image, Ada.Strings.Left));

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

end Password_Manager;
