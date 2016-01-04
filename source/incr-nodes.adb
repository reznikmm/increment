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

with Incr.Nodes.Tokens;

package body Incr.Nodes is

   To_Diff  : constant array (Boolean) of Integer :=
     (False => -1, True => 1);

   ------------------
   -- Constructors --
   ------------------

   package body Constructors is

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize (Self : aliased in out Node_With_Parent'Class) is
      begin
         Versioned_Booleans.Initialize (Self.Exist, False);
         Versioned_Nodes.Initialize (Self.Parent, null);
      end Initialize;

      ------------------------
      -- Initialize_Ancient --
      ------------------------

      procedure Initialize_Ancient
        (Self    : aliased in out Node_With_Parent'Class;
         Parent  : Node_Access) is
      begin
         Versioned_Booleans.Initialize (Self.Exist, True);
         Versioned_Nodes.Initialize (Self.Parent, Parent);
      end Initialize_Ancient;

   end Constructors;

   ------------
   -- Exists --
   ------------

   overriding function Exists
     (Self  : Node_With_Exist;
      Time  : Version_Trees.Version) return Boolean is
   begin
      return Versioned_Booleans.Get (Self.Exist, Time);
   end Exists;

   -----------------
   -- First_Token --
   -----------------

   function First_Token
     (Self : aliased in out Node'Class;
      Time : Version_Trees.Version)
      return Tokens.Token_Access
   is
      Child : Node_Access;
   begin
      if Self.Arity > 0 then
         Child := Self.Child (1, Time);

         if Child.Is_Token then
            return Tokens.Token_Access (Child);
         else
            return Child.First_Token (Time);
         end if;
      elsif Self.Is_Token then
         return Tokens.Token'Class (Self)'Access;
      else
         return null;
      end if;
   end First_Token;

   --------------
   -- Get_Flag --
   --------------

   overriding function Get_Flag
     (Self  : Node_With_Exist;
      Flag  : Transient_Flags) return Boolean is
   begin
      return Self.Flag (Flag);
   end Get_Flag;

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token
     (Self : aliased in out Node'Class;
      Time : Version_Trees.Version) return Tokens.Token_Access
   is
      Child : Node_Access;
   begin
      if Self.Arity > 0 then
         Child := Self.Child (Self.Arity, Time);

         if Child.Is_Token then
            return Tokens.Token_Access (Child);
         else
            return Child.Last_Token (Time);
         end if;
      elsif Self.Is_Token then
         return Tokens.Token'Class (Self)'Access;
      else
         return null;
      end if;
   end Last_Token;

   ------------------
   -- Next_Subtree --
   ------------------

   function Next_Subtree
     (Self : Node'Class;
      Time : Version_Trees.Version) return Node_Access
   is
      type Constant_Node_Access is access constant Node'Class;
      Parent : Node_Access := Self.Parent (Time);
      Child  : Node_Access;
   begin
      while Parent /= null loop

         for J in 1 .. Parent.Arity - 1 loop
            Child := Parent.Child (J, Time);

            if Constant_Node_Access (Child) = Self'Unchecked_Access then
               for K in J + 1 .. Parent.Arity loop
                  Child := Parent.Child (K, Time);

                  if Child /= null then
                     return Child;
                  end if;
               end loop;
            end if;
         end loop;

         Parent := Parent.Parent (Time);
      end loop;

      return null;
   end Next_Subtree;

   ---------------
   -- On_Commit --
   ---------------

   overriding procedure On_Commit (Self : in out Node_With_Exist) is
   begin
      Self.Nested_Changes := 0;
      Self.Local_Changes := 0;
      Self.Flag := (others => False);
   end On_Commit;

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self  : Node_With_Parent;
      Time  : Version_Trees.Version)
      return Node_Access is
   begin
      return Versioned_Nodes.Get (Self.Parent, Time);
   end Parent;


   ----------------------
   -- Previous_Subtree --
   ----------------------

   function Previous_Subtree
     (Self : Node'Class;
      Time : Version_Trees.Version) return Node_Access
   is
      type Constant_Node_Access is access constant Node'Class;
      Parent : Node_Access := Self.Parent (Time);
      Child  : Node_Access;
   begin
      while Parent /= null loop

         for J in 2 .. Parent.Arity loop
            Child := Parent.Child (J, Time);

            if Constant_Node_Access (Child) = Self'Unchecked_Access then
               return Parent.Child (J - 1, Time);
            end if;
         end loop;

         Parent := Parent.Parent (Time);
      end loop;

      return null;
   end Previous_Subtree;

   ------------------------------
   -- Propagate_Nested_Changes --
   ------------------------------

   procedure Propagate_Nested_Changes
     (Self : in out Node'Class;
      Diff : Integer)
   is
      Parent : constant Node_Access :=
        Self.Parent (Self.Document.History.Changing);
   begin
      if Parent /= null then
         Parent.On_Nested_Changes (Diff);
      end if;
   end Propagate_Nested_Changes;

   ------------------------------
   -- Propagate_Nested_Changes --
   ------------------------------

   overriding procedure On_Nested_Changes
     (Self : in out Node_With_Exist;
      Diff : Integer)
   is
      Before : Boolean;
      After  : Boolean;
   begin
      Before := Self.Local_Changes > 0 or Self.Nested_Changes > 0;
      Self.Nested_Changes := Self.Nested_Changes + Diff;
      After := Self.Local_Changes > 0 or Self.Nested_Changes > 0;

      if Before /= After then
         Self.Propagate_Nested_Changes (To_Diff (After));
      end if;
   end On_Nested_Changes;

   --------------
   -- Set_Flag --
   --------------

   overriding procedure Set_Flag
     (Self  : in out Node_With_Exist;
      Flag  : Transient_Flags;
      Value : Boolean := True)
   is
      Before : Boolean;
      After  : Boolean;
   begin
      Before := (Self.Flag and Local_Changes_Mask) /= No_Flags;
      Self.Flag (Flag) := Value;
      After := (Self.Flag and Local_Changes_Mask) /= No_Flags;

      if Before /= After then
         Self.Update_Local_Changes (To_Diff (After));
      end if;
   end Set_Flag;

   ----------------
   -- Set_Parent --
   ----------------

   overriding procedure Set_Parent
     (Self  : in out Node_With_Parent;
      Value : Node_Access)
   is
      Changed : Boolean;
      Ignore  : Integer;
      Now     : constant Version_Trees.Version :=
        Self.Document.History.Changing;
   begin
      Changed := Self.Local_Changes > 0 or Self.Nested_Changes > 0;

      if Changed then
         Self.Propagate_Nested_Changes (-1);
      end if;

      Versioned_Nodes.Set (Self.Parent, Value, Now, Ignore);

      if Changed then
         Self.Propagate_Nested_Changes (1);
      end if;
   end Set_Parent;

   --------------------------
   -- Update_Local_Changes --
   --------------------------

   not overriding procedure Update_Local_Changes
     (Self : in out Node_With_Exist;
      Diff : Integer)
   is
      Before : Boolean;
      After  : Boolean;
   begin
      Before := Self.Local_Changes > 0 or Self.Nested_Changes > 0;
      Self.Local_Changes := Self.Local_Changes + Diff;
      After := Self.Local_Changes > 0 or Self.Nested_Changes > 0;

      if Before /= After then
         Self.Propagate_Nested_Changes (To_Diff (After));
      end if;
   end Update_Local_Changes;

end Incr.Nodes;
