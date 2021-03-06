--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Incr.Version_Trees is

   ----------------------
   -- Versioned_Values --
   ----------------------

   package body Versioned_Values is

      -------------
      -- Discard --
      -------------

      procedure Discard
        (Self    : in out Container;
         Time    : Version;
         Changes : out Integer) is
      begin
         if Time = Self.Versions (Self.Index) then
            Self.Index := Self.Index - 1;
            Changes := -1;
         elsif Time < Self.Versions (Self.Index) then
            --  Reverting of earlyer version is not allowed
            raise Constraint_Error;
         else
            Changes := 0;
         end if;
      end Discard;

      ---------
      -- Get --
      ---------

      function Get (Self : Container; Time : Version) return Element is
      begin
         for J in Self.Versions'Range loop
            if Time >= Self.Versions (Self.Index - J) then
               return Self.Elements (Self.Index - J);
            end if;
         end loop;

         raise Constraint_Error with "version is too old";
      end Get;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Self          : in out Container;
         Initial_Value : Element)
      is
      begin
         Self.Versions := (others => 0);
         Self.Elements := (others => Initial_Value);
         Self.Index := 0;
      end Initialize;

      ---------
      -- Set --
      ---------

      procedure Set
        (Self    : in out Container;
         Value   : Element;
         Time    : Version;
         Changes : in out Integer)
      is
         Prev   : constant Version := Time - 1;  -- Tree.Parent (Time)
         Old    : constant Element := Get (Self, Prev);
         Is_Old : constant Boolean := Value = Old;
      begin
         if Time = Self.Versions (Self.Index) and Is_Old then
            Self.Index := Self.Index - 1;
            Changes := Changes - 1;
         elsif Time > Self.Versions (Self.Index) and not Is_Old then
            Self.Index := Self.Index + 1;
            Self.Versions (Self.Index) := Time;
            Changes := Changes + 1;
         elsif Time < Self.Versions (Self.Index) then
            --  Update of earlyer version is not allowed
            raise Constraint_Error;
         end if;

         if not Is_Old then
            Self.Elements (Self.Index) := Value;
         end if;
      end Set;

   end Versioned_Values;

   function "<" (Left, Right : Version) return Boolean is
      L : constant Natural := Natural (Left);
      R : constant Natural := Natural (Right);
   begin
      if L < 128 xor R < 128 then
         return Natural (L + 8) < Natural (R + 8);
      else
         return L < R;
      end if;
   end "<";

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

   ------------
   -- Parent --
   ------------

   function Parent (Self : Version_Tree; Value : Version) return Version is
      pragma Unreferenced (Self);
   begin
      return Value - 1;
   end Parent;

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
