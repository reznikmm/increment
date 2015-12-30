------------------------------------------------------------------------------
--                                                                          --
--                               Gela Project                               --
--                                                                          --
--                Programming Language Construction Framework               --
--                                                                          --
--                      Incremental Analysis Component                      --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2015, Maxim Reznik <max@gela.work>                           --
-- All rights reserved.                                                     --
--                                                                          --
-- Redistribution and use in source and binary forms, with or without       --
-- modification, are permitted provided that the following conditions       --
-- are met:                                                                 --
--                                                                          --
--  * Redistributions of source code must retain the above copyright        --
--    notice, this list of conditions and the following disclaimer.         --
--                                                                          --
--  * Redistributions in binary form must reproduce the above copyright     --
--    notice, this list of conditions and the following disclaimer in the   --
--    documentation and/or other materials provided with the distribution.  --
--                                                                          --
--  * Neither the name of the Maxim Reznik, IE nor the names of its         --
--    contributors may be used to endorse or promote products derived from  --
--    this software without specific prior written permission.              --
--                                                                          --
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      --
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        --
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR    --
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT     --
-- HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED --
-- TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR   --
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF   --
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     --
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS       --
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.             --
--                                                                          --
------------------------------------------------------------------------------
--  $Revision$ $Date$
------------------------------------------------------------------------------

package body Incr.Version_Trees is

   ----------------------
   -- Versioned_Values --
   ----------------------

   package body Versioned_Values is

      ---------
      -- Get --
      ---------

      function Get
        (Self : Container;
         Time : Version)
         return Element
      is
         Low  : Positive := Self.Versions'First;
         High : Positive := Self.Index;
         Mid  : Positive;
      begin
         if Time >= Self.Versions (Self.Index) then
            return Self.Elements (Self.Index);
         end if;

         loop
            Mid := (Low + High) / 2;

            if Time < Self.Versions (Mid) then
               if Low = Mid then
                  return Self.Elements (Mid);
               end if;

               High := Mid - 1;
            elsif Time > Self.Versions (Mid) then
               if High = Mid then
                  return Self.Elements (Mid - 1);
               end if;

               Low := Mid + 1;
            else
               return Self.Elements (Mid);
            end if;
         end loop;
      end Get;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Self          : in out Container;
         Initial_Value : Element)
      is
      begin
         Self.Versions (1) := 0;
         Self.Elements (1) := Initial_Value;
         Self.Index := 1;
      end Initialize;

      ---------
      -- Set --
      ---------

      procedure Set
        (Self    : in out Container;
         Value   : Element;
         Time    : Version;
         Changes : out Integer)
      is
         Prev : constant Version := Time - 1;  -- Tree.Parent (Time)
         Old  : constant Element := Get (Self, Prev);
      begin
         if Time = Self.Versions (Self.Index) and Value = Old then
            Self.Index := Self.Index - 1;
            Changes := -1;
         elsif Time > Self.Versions (Self.Index) and Value /= Old then
            Self.Index := Self.Index + 1;
            Self.Versions (Self.Index) := Time;
            Changes := 1;
         elsif Time < Self.Versions (Self.Index) then
            --  Update of earlyer version is not allowed
            raise Constraint_Error;
         else
            Changes := 0;
         end if;

         if Value /= Old then
            Self.Elements (Self.Index) := Value;
         end if;
      end Set;

   end Versioned_Values;

   --------------
   -- Changing --
   --------------

   function Changing (Self : Version_Tree) return Version is
   begin
      return Self.Changing;
   end Changing;

   -----------------
   -- Is_Changing --
   -----------------

   function Is_Changing
     (Self : Version_Tree; Value : Version) return Boolean is
   begin
      return Self.Changing = Value;
   end Is_Changing;

   --------------------
   -- Is_Prehistoric --
   --------------------

   function Is_Prehistoric
     (Self : Version_Tree; Value : Version) return Boolean is
   begin
      return Self.Prehistoric = Value;
   end Is_Prehistoric;

   ------------
   -- Parent --
   ------------

   function Parent (Self : Version_Tree; Value : Version) return Version is
      pragma Unreferenced (Self);
   begin
      return Value - 1;
   end Parent;

   -----------------
   -- Prehistoric --
   -----------------

   function Prehistoric (Self : Version_Tree) return Version is
      pragma Unreferenced (Self);
   begin
      return 0;
   end Prehistoric;

   ------------------
   -- Start_Change --
   ------------------

   procedure Start_Change
     (Self     : in out Version_Tree;
      Parent   : Version;
      Changing : out Version)
   is
   begin
      if Parent /= Self.Changing then
         raise Constraint_Error with "not implemented";
      end if;

      Self.Changing := Self.Changing + 1;
      Changing := Self.Changing;
   end Start_Change;

end Incr.Version_Trees;
