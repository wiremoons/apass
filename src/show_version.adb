-------------------------------------------------------------------------------
-- Package     : Show_Version                                                --
-- Description : Display current program version and build info.             --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Maps;         use Ada.Strings.Maps;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Command_Line;
with GNAT.Source_Info;
with GNAT.Compiler_Version;
with System.Multiprocessors;

package body Show_Version is

   --  SET APPLICATION VERSION TO DISPLAY BELOW  --
   AppVersion : constant String := "0.0.5";

   package CVer is new GNAT.Compiler_Version;
   --  Linux distros using 'systemd' are required to have the file:
   OS_Release_File : constant String := "/etc/os-release";
   F               : File_Type;

   procedure Set_Debug (Is_Debug : in out Boolean) is
   --------------------------------------
   -- Set if in debug build
   --------------------------------------
   begin
      --  Only gets called if program is compiled as a 'debug' build
      --  so variable only set 'true' if this is the case
      Is_Debug := True;
   end Set_Debug;

   function Is_Linux return Boolean is
   ---------------------------------------
   --  Check if the OS is a Linux distro
   ---------------------------------------
   begin
      if Ada.Directories.Exists (OS_Release_File) then
         return True;
      else
         return False;
      end if;
   end Is_Linux;

   function Is_Windows return Boolean is
   ---------------------------------------
   --  Check if the OS is Windows
   ---------------------------------------
   --  alter to use env variable in Windiws is not on a 'c:' drive
   --
   begin
      if Ada.Directories.Exists ("c:\windows") then
         return True;
      else
         return False;
      end if;
   end Is_Windows;

   procedure Clean_Pretty_Name (OS_Name : in out Unbounded_String) is
      -----------------------------------------------
      --  Clean up the 'PRETTY_NAME' and extract text
      -----------------------------------------------
      --  Expects the line of text in the format:
      --    PRETTY_NAME="Ubuntu 20.04.1 LTS"
      --
      --  Delete all leading text up to and including '=' leaving:
      --    "Ubuntu 20.04.1 LTS"
      --
      --  This is then further cleaned up to remove both '"' characters which
      --  are defined in the 'Quote_Char' Character_Set. The cleaned up text is
      --  modified in place using the provided Unbounded.String resulting in a
      --  final string of (or equivalent for other PRETTY_NAME entries):
      --    Ubuntu 20.04.1 LTS

      Quote_Char : constant Character_Set := To_Set ('"'); --\""

   begin
      if Length (OS_Name) > 0 then
         --  delete up to (and including) character '=' in string
         Delete (OS_Name, 1, Index (OS_Name, "="));
         -- trim off quotes
         Trim (OS_Name, Quote_Char, Quote_Char);
      end if;

   end Clean_Pretty_Name;

   function Get_Linux_OS return String is
      ----------------------------------------
      --  Get the OS Linux distro 'PRETTY_NAME'
      ----------------------------------------
      --  Linux systems running 'systemd' should include a file:
      --    /etc/os-release
      --
      --  This file includes many entries but should have the line:
      --     PRETTY_NAME="Ubuntu 20.04.1 LTS"

      --  The file is opened and read until the above line is located. The
      --  line is then cleaned up in the procedure 'Clean_Pretty_Name'. The
      --  remaining text should jus be:
      --     Ubuntu 20.04.1 LTS
      --  that is returned as a String.

      OS_Name : Unbounded_String := Null_Unbounded_String;

   begin
      if Ada.Directories.Exists (OS_Release_File) then
         Open (F, In_File, OS_Release_File);
         while not End_Of_File (F) loop
            declare
               Line : constant String := Get_Line (F);
            begin
               if Ada.Strings.Fixed.Count (Line, "PRETTY_NAME") > 0 then

                  --  get the identified line from the file
                  OS_Name := To_Unbounded_String (Line);
                  pragma Debug (New_Line (Standard_Error, 1));
                  pragma Debug (Put_Line (Standard_Error, "[DEBUG] unmodified: " & OS_Name));

                  -- extract the part required
                  Clean_Pretty_Name (OS_Name);
                  pragma Debug (Put_Line (Standard_Error, "[DEBUG] cleaned up: " & OS_Name));
               end if;
            end;
         end loop;
         --  return the extracted distro text
         return To_String (OS_Name);

      else
         New_Line (2);
         Put_Line (Standard_Error, "ERROR: unable to locate file:");
         Put_Line (Standard_Error, "  - " & OS_Release_File);
         New_Line (1);
         return "UNKNOWN LINUX OS";
      end if;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         New_Line (2);
         Put_Line (Standard_Error, "ERROR: file not found exception!");
         return "UNKNOWN LINUX OS";
      when others =>
         New_Line (2);
         Put_Line (Standard_Error, "ERROR: unknown exception!");
         return "UNKNOWN LINUX OS";

   end Get_Linux_OS;

   procedure Show is
      -------------------------------------------
      --  Collect and display version information
      -------------------------------------------
      Is_Debug : Boolean := False;

   begin
      --  only gets called if complied with: '-gnata'
      pragma Debug (Set_Debug (Is_Debug));
      pragma Debug (Put_Line (Standard_Error, "[DEBUG] 'Show_Version' is running in debug mode."));
      --  start output of version information
      New_Line (1);
      Put ("'");
      Put (Ada.Directories.Simple_Name (Ada.Command_Line.Command_Name));
      Put ("' is version: '");
      Put (AppVersion);
      Put ("' running on: '");
      if Is_Linux then
         Put (Get_Linux_OS);
      elsif Is_Windows then
         Put ("Windows");
      else
         Put ("UNKNOWN OS");
      end if;
      Put ("' with");
      Put (System.Multiprocessors.Number_Of_CPUs'Image);
      Put_Line (" CPU cores.");
      Put ("Compiled on: ");
      Put (GNAT.Source_Info.Compilation_ISO_Date);
      Put (" @ ");
      Put (GNAT.Source_Info.Compilation_Time);
      Put_Line (".");
      Put_Line ("Copyright (c) 2021 Simon Rowe.");
      New_Line (1);
      Put ("Ada source built as '");
      if Is_Debug then
         Put ("debug");
      else
         Put ("release");
      end if;
      Put ("' using GNAT complier version: '");
      Put (CVer.Version);
      Put_Line ("'.");
      New_Line (1);
      Put_Line ("For licenses and further information visit:");
      Put_Line (" - https://github.com/wiremoons/apass/");
      New_Line (1);

   end Show;

end Show_Version;
