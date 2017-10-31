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

with Incr.Nodes.Joints;

package body Incr.Nodes.Ultra_Roots is

   -----------
   -- Arity --
   -----------

   overriding function Arity (Self : Ultra_Root) return Natural is
      pragma Unreferenced (Self);
   begin
      return 3;
   end Arity;

   -----------
   -- Child --
   -----------

   overriding function Child
     (Self  : Ultra_Root;
      Index : Positive;
      Time  : Version_Trees.Version)
      return Node_Access
   is
   begin
      case Index is
         when 1 =>
            return Node_Access (Self.BOS);
         when 2 =>
            return Versioned_Nodes.Get (Self.Root, Time);
         when 3 =>
            return Node_Access (Self.EOS);
         when others =>
            raise Constraint_Error;
      end case;
   end Child;

   -------------
   -- Discard --
   -------------

   overriding procedure Discard (Self  : in out Ultra_Root) is
   begin
      raise Program_Error with "Unimplemented";
   end Discard;

   ------------
   -- Exists --
   ------------

   overriding function Exists
     (Self  : Ultra_Root;
      Time  : Version_Trees.Version) return Boolean
   is
      pragma Unreferenced (Time, Self);
   begin
      return True;
   end Exists;

   --------------
   -- Get_Flag --
   --------------

   overriding function Get_Flag
     (Self  : Ultra_Root;
      Flag  : Transient_Flags) return Boolean is
      pragma Unreferenced (Self, Flag);
   begin
      return True;
   end Get_Flag;

   --------------
   -- Is_Token --
   --------------

   overriding function Is_Token (Self : Ultra_Root) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Token;

   ----------
   -- Kind --
   ----------

   overriding function Kind (Self : Ultra_Root) return Node_Kind is
      pragma Unreferenced (Self);
   begin
      return 0;
   end Kind;

   -------------------
   -- Local_Changes --
   -------------------

   overriding function Local_Changes
     (Self : Ultra_Root;
      From : Version_Trees.Version;
      To   : Version_Trees.Version) return Boolean
   is
      use type Version_Trees.Version;
      Next : Version_Trees.Version := To;
      Prev : Version_Trees.Version;
   begin
      loop
         Prev := Self.Document.History.Parent (Next);

         if Self.Child (2, Prev) /= Self.Child (2, Next) then
            return True;
         end if;

         exit when From = Prev;

         Next := Prev;
      end loop;

      return False;
   end Local_Changes;

   -------------------
   -- Nested_Errors --
   -------------------

   overriding function Nested_Errors
     (Self : Ultra_Root;
      Time : Version_Trees.Version) return Boolean
   is
      Child : Nodes.Node_Access;
   begin
      for J in 1 .. 3 loop
         Child := Self.Child (J, Time);

         if Child /= null then
            if Child.Local_Errors (Time) or Child.Nested_Errors (Time) then
               return True;
            end if;
         end if;
      end loop;

      return False;
   end Nested_Errors;

   --------------------
   -- Nested_Changes --
   --------------------

   overriding function Nested_Changes
     (Self : Ultra_Root;
      From : Version_Trees.Version;
      To   : Version_Trees.Version) return Boolean
   is
      pragma Unreferenced (Self, From, To);
   begin
      return True;
   end Nested_Changes;

   ---------------
   -- On_Commit --
   ---------------

   overriding procedure On_Commit (Self : in out Ultra_Root) is
      Root : constant Node_Access :=
        Versioned_Nodes.Get (Self.Root, Self.Document.History.Changing);
   begin
      Self.BOS.On_Commit;

      if Root /= null then
         Root.On_Commit;
      end if;

      Self.EOS.On_Commit;
   end On_Commit;

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self  : Ultra_Root;
      Time  : Version_Trees.Version) return Node_Access
   is
      pragma Unreferenced (Self, Time);
   begin
      return null;
   end Parent;

   ---------------
   -- Set_Child --
   ---------------

   overriding procedure Set_Child
     (Self  : aliased in out Ultra_Root;
      Index : Positive;
      Value : Node_Access)
   is
      Now : constant Version_Trees.Version := Self.Document.History.Changing;

      Ignore : Integer := 0;
   begin
      if Index /= 2 then
         raise Constraint_Error;
      end if;

      Versioned_Nodes.Set (Self.Root, Value, Now, Ignore);

      if Value /= null then
         Value.Set_Parent (Self'Unchecked_Access);
      end if;
   end Set_Child;

   ----------
   -- Span --
   ----------

   overriding function Span
     (Self : aliased in out Ultra_Root;
      Kind : Span_Kinds;
      Time : Version_Trees.Version) return Natural
   is
      Root_Span : constant Natural := Self.Child (2, Time).Span (Kind, Time);
   begin
      case Kind is
         when Text_Length =>
            return Root_Span + 1;  --  including end_of_stream character
         when Token_Count =>
            return Root_Span + 2;  --  including sentinel tokens
         when Line_Count =>
            return Root_Span + 1;  --  including end_of_stream character
      end case;
   end Span;

   ------------------
   -- Constructors --
   ------------------

   package body Constructors is

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize (Self  : out Ultra_Root'Class) is
         Root : constant Incr.Nodes.Joints.Joint_Access := null;
      begin
         Self.BOS := new Nodes.Tokens.Token (Self.Document);
         Nodes.Tokens.Constructors.Initialize_Ancient
           (Self   => Self.BOS.all,
            Parent => Self'Unchecked_Access);

         Self.EOS := new Nodes.Tokens.Token (Self.Document);
         Nodes.Tokens.Constructors.Initialize_Ancient
           (Self   => Self.EOS.all,
            Parent => Self'Unchecked_Access);

         Versioned_Nodes.Initialize (Self.Root, Nodes.Node_Access (Root));
      end Initialize;

   end Constructors;

end Incr.Nodes.Ultra_Roots;
