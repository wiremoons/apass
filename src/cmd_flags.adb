-------------------------------------------------------------------------------
-- Package     : Cmd_Flags                                                   --
-- Description : Manage user provided CLI flags for the program.             --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------

with Ada.Text_IO;       use Ada.Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;
-- local packages
with Show_Version;

package body Cmd_Flags is

   function Command_Line_Flags_Exist return Boolean is
      ----------------------------------------------
      --  Parse and manage and command line flags --
      ----------------------------------------------

      --  GNAT.Command_Line variables and config
      Help_Option    : aliased Boolean := False;
      Version_Option : aliased Boolean := False;
      Config         : Command_Line_Configuration;

   begin
      --  define params for the 'help' option
      Define_Switch
        (Config, Help_Option'Access, Switch => "-h", Long_Switch => "--help",
         Help                               => "Show command line usage for application");
      --  define params for the 'version' option
      Define_Switch
        (Config, Version_Option'Access, Switch => "-v", Long_Switch => "--version", Help => "Show version details");
      --  Additional help message as first line of 'Display_Help()'
      Set_Usage
        (Config, Usage => "[switches] [arguments]",
         Help          => "Program to generate passwords from random three letter words.");

      --  cli flags parse using config and above defined switched
      Getopt (Config);

      --  show program 'version' if requested
      if Version_Option then
         Show_Version.Show;
         return True;
      end if;

      --  show 'help' if requested
      if Help_Option then
         New_Line (1);
         Display_Help (Config);
         return True;
      end if;

      --  no cli params : so just return
      --Try_Help;  -- alternative one line response
      return False;

   exception
      when Invalid_Switch =>
         New_Line (1);
         Put_Line (Standard_Error, "Exception caught: caused by the use of an invalid command line switch.");
         New_Line (1);
         Display_Help (Config);
         return True;
      when Invalid_Parameter =>
         New_Line (1);
         Put_Line
           (Standard_Error, "Exception caught: caused by the use of an invalid parameter to a command line switch.");
         New_Line (1);
         Display_Help (Config);
         return True;
      when Exit_From_Command_Line =>
         New_Line (1);
         Put_Line (Standard_Error, "Exit following display of help message.");
         return True;

   end Command_Line_Flags_Exist;

end Cmd_Flags;
