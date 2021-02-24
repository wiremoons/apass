-------------------------------------------------------------------------------
-- Package     : Show_Version                                                --
-- Description : Display current program version and build info.             --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------

package Show_Version is

   procedure Show;
   --  Main procedure to output program version information

   procedure Set_Debug (Is_Debug : in out Boolean);
   --  Test if application is a debug build

   function Is_Linux return Boolean;
   --  Check if running on a Linux operating system

   function Is_Windows return Boolean;
   --  Check if running on a Windows operating system

   function Get_Linux_OS return String;
   --  Get the name and version of the running Linux operating system

end Show_Version;
