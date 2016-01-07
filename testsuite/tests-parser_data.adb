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

   Action_Data : aliased constant P.Action_Table :=
     (1 => ((P.Reduce, 2), (P.Shift, 4),  (P.Shift, 3),  (P.Shift, 2)),
      2 => (0 .. 3 => (P.Reduce, 4)),
      3 => (0 .. 3 => (P.Reduce, 5)),
      4 => (0 .. 3 => (P.Reduce, 3)),
      5 => (0 .. 3 => (Kind => P.Finish)),
      6 => (0 .. 3 => (P.Reduce, 7)),
      7 => ((P.Reduce, 1), (P.Shift, 4),  (P.Shift, 3),  (P.Shift, 2)),
      8 => (0 .. 3 => (P.Reduce, 6)));


   State_Data : aliased constant P.State_Table :=
     (1 => (1 => 5, 2 => 6, 3 => 7),
      2 .. 6 => (1 .. 3 => 0),
      7 => (1 => 0, 2 => 8, 3 => 0),
      8 => (1 .. 3 => 0));

   Count_Data : aliased constant P.Parts_Count_Table :=
     (1 => 1, 2 => 0, 3 => 1, 4 => 1, 5 => 1, 6 => 2, 7 => 1);

   NT : constant array (P.Production_Index range 1 .. 7) of
     Incr.Nodes.Node_Kind :=
       (1 .. 2 => 1, 3 .. 5 => 2, 6 .. 7 => 3);

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

   overriding function Create_Node
     (Self     : aliased in out Node_Factory;
      Prod     : P.Production_Index;
      Children : Incr.Nodes.Node_Array) return Incr.Nodes.Node_Access
   is
      Kind   : constant Incr.Nodes.Node_Kind := NT (Prod);
      Result : constant Incr.Nodes.Joints.Joint_Access :=
        new Incr.Nodes.Joints.Joint (Self.Document, Children'Length);
   begin
      Incr.Nodes.Joints.Constructors.Initialize (Result.all, Kind, Children);

      return Incr.Nodes.Node_Access (Result);
   end Create_Node;

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
