-------------------------------------------------------------------------------
-- BUILD FILE  : apass                                                       --
-- Description : A CLI password generator written in Ada.                    --
-- Author      : Simon Rowe <simon@wiremoons.com>                            --
-- License     : MIT Open Source.                                            --
-------------------------------------------------------------------------------

package Password_Manager is

   function Total_Words return String;
   function Basic_Password (Number_Of_Words : Positive := 3) return String;

end Password_Manager;
