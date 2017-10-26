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

package body Tests.Parser_Data is

   S : constant P.Action_Kinds := P.Shift;
   R : constant P.Action_Kinds := P.Reduce;
   E : constant P.Action := (Kind => P.Error);
   F : constant P.Action := (Kind => P.Finish);

   Action_Data : aliased constant P.Action_Table :=
     (1 => ((R, 2), (S, 4), (S, 3), (S, 2), (S, 5), (S, 6), (S, 7)),
      2 => ((R, 4), (R, 4), (R, 4), (R, 4), E, (R, 4), E),
      3 => ((R, 5), (R, 5), (R, 5), (R, 5), E, (R, 5), E),
      4 => ((R, 3), (R, 3), (R, 3), (R, 3), E, (R, 3), E),
      5 => (F, E, E, E, E, E, E),
      6 => ((R, 7), (R, 7), (R, 7), (R, 7), E, (R, 7), E),
      7 => ((R, 1), (S, 4), (S, 3), (S, 2), E, (S, 8), E),
      8 => ((R, 6), (R, 6), (R, 6), (R, 6), E, (R, 6), E));

   State_Data  : aliased constant P.State_Table :=
     (1 => (4 => 5, 5 => 6, 6 => 7), 2 => (4 => 0, 5 => 0, 6 => 0),
      3 => (4 => 0, 5 => 0, 6 => 0), 4 => (4 => 0, 5 => 0, 6 => 0),
      5 => (4 => 0, 5 => 0, 6 => 0), 6 => (4 => 0, 5 => 0, 6 => 0),
      7 => (4 => 0, 5 => 8, 6 => 0), 8 => (4 => 0, 5 => 0, 6 => 0));

   Count_Data  : aliased constant P.Parts_Count_Table :=
     (1 => 1, 2 => 0, 3 => 1, 4 => 1, 5 => 1, 6 => 2, 7 => 1);

   NT          : constant Node_Kind_Array :=
     (1 .. 2 => 4, 3 .. 5 => 5, 6 .. 7 => 6);

   -------------
   -- Actions --
   -------------

   overriding function Actions
     (Self : Provider) return P.Action_Table_Access
   is
      pragma Unreferenced (Self);
   begin
      return Action_Data'Access;
   end Actions;

   -----------------
   -- Create_Node --
   -----------------

   overriding procedure Create_Node
     (Self     : aliased in out Node_Factory;
      Prod     : Incr.Parsers.Incremental.
        Parser_Data_Providers.Production_Index;
      Children : Incr.Nodes.Node_Array;
      Node     : out Incr.Nodes.Node_Access;
      Kind     : out Incr.Nodes.Node_Kind)
   is
      Result : Incr.Nodes.Joints.Joint_Access;
   begin
      Kind := NT (Prod);

      if Children'Length = 0 then
         Node := null;
         return;
      end if;

      Result := new Incr.Nodes.Joints.Joint (Self.Document, Children'Length);
      Incr.Nodes.Joints.Constructors.Initialize (Result.all, Kind, Children);

      Node := Incr.Nodes.Node_Access (Result);
   end Create_Node;

   overriding function Kind_Image
     (Self : Provider;
      Kind : Incr.Nodes.Node_Kind) return Wide_Wide_String
   is
      pragma Unreferenced (Self);
   begin
      case Kind is
         when 0 =>
            return "EOF";
         when 1 =>
            return "Ident";
         when 2 =>
            return "New_Line";
         when 3 =>
            return "Number";
         when 4 =>
            return "list";
         when 5 =>
            return "tok";
         when 6 =>
            return "tok_list";
         when others =>
            return "unknown";
      end case;
   end Kind_Image;

   ------------
   -- States --
   ------------

   overriding function States
     (Self : Provider) return P.State_Table_Access
   is
      pragma Unreferenced (Self);
   begin
      return State_Data'Access;
   end States;

   -----------------
   -- Part_Counts --
   -----------------

   overriding function Part_Counts
     (Self : Provider) return P.Parts_Count_Table_Access
   is
      pragma Unreferenced (Self);
   begin
      return Count_Data'Access;
   end Part_Counts;

end Tests.Parser_Data;
