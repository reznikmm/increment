--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Wide_Wide_Text_IO;

with League.Strings;

with Incr.Ada_Lexers;
with Incr.Lexers.Batch_Lexers;

procedure Ada_Lexer_Test is
   type File_Source is new Incr.Lexers.Batch_Lexers.Abstract_Source with record
      Text  : League.Strings.Universal_String;
      Index : Positive := 1;
   end record;

   overriding function Get_Next (Self : not null access File_Source)
     return Wide_Wide_Character;

   function Debug_Image
     (Value : Incr.Ada_Lexers.Token) return Wide_Wide_String;

   function Debug_Image
     (Value : Incr.Ada_Lexers.Token) return Wide_Wide_String
   is
      use Incr.Ada_Lexers;
   begin
      case Value is
         when Arrow_Token => return "Arrow_Token";
         when Double_Dot_Token => return "Double_Dot_Token";
         when Double_Star_Token => return "Double_Star_Token";
         when Assignment_Token => return "Assignment_Token";
         when Inequality_Token => return "Inequality_Token";
         when Greater_Or_Equal_Token => return "Greater_Or_Equal_Token";
         when Less_Or_Equal_Token => return "Less_Or_Equal_Token";
         when Left_Label_Token => return "Left_Label_Token";
         when Right_Label_Token => return "Right_Label_Token";
         when Box_Token => return "Box_Token";
         when Ampersand_Token => return "Ampersand_Token";
         when Apostrophe_Token => return "Apostrophe_Token";
         when Left_Parenthesis_Token => return "Left_Parenthesis_Token";
         when Right_Parenthesis_Token => return "Right_Parenthesis_Token";
         when Star_Token => return "Star_Token";
         when Plus_Token => return "Plus_Token";
         when Comma_Token => return "Comma_Token";
         when Hyphen_Token => return "Hyphen_Token";
         when Dot_Token => return "Dot_Token";
         when Slash_Token => return "Slash_Token";
         when Colon_Token => return "Colon_Token";
         when Semicolon_Token => return "Semicolon_Token";
         when Less_Token => return "Less_Token";
         when Equal_Token => return "Equal_Token";
         when Greater_Token => return "Greater_Token";
         when Vertical_Line_Token => return "Vertical_Line_Token";
         when Identifier_Token => return "Identifier_Token";
         when Numeric_Literal_Token => return "Numeric_Literal_Token";
         when Character_Literal_Token => return "Character_Literal_Token";
         when String_Literal_Token => return "String_Literal_Token";
         when Comment_Token => return "Comment_Token";
         when Space_Token => return "Space_Token";
         when New_Line_Token => return "New_Line_Token";
         when Error_Token => return "Error_Token";
         when Abort_Token => return "Abort_Token";
         when Abs_Token => return "Abs_Token";
         when Abstract_Token => return "Abstract_Token";
         when Accept_Token => return "Accept_Token";
         when Access_Token => return "Access_Token";
         when Aliased_Token => return "Aliased_Token";
         when All_Token => return "All_Token";
         when And_Token => return "And_Token";
         when Array_Token => return "Array_Token";
         when At_Token => return "At_Token";
         when Begin_Token => return "Begin_Token";
         when Body_Token => return "Body_Token";
         when Case_Token => return "Case_Token";
         when Constant_Token => return "Constant_Token";
         when Declare_Token => return "Declare_Token";
         when Delay_Token => return "Delay_Token";
         when Delta_Token => return "Delta_Token";
         when Digits_Token => return "Digits_Token";
         when Do_Token => return "Do_Token";
         when Else_Token => return "Else_Token";
         when Elsif_Token => return "Elsif_Token";
         when End_Token => return "End_Token";
         when Entry_Token => return "Entry_Token";
         when Exception_Token => return "Exception_Token";
         when Exit_Token => return "Exit_Token";
         when For_Token => return "For_Token";
         when Function_Token => return "Function_Token";
         when Generic_Token => return "Generic_Token";
         when Goto_Token => return "Goto_Token";
         when If_Token => return "If_Token";
         when In_Token => return "In_Token";
         when Interface_Token => return "Interface_Token";
         when Is_Token => return "Is_Token";
         when Limited_Token => return "Limited_Token";
         when Loop_Token => return "Loop_Token";
         when Mod_Token => return "Mod_Token";
         when New_Token => return "New_Token";
         when Not_Token => return "Not_Token";
         when Null_Token => return "Null_Token";
         when Of_Token => return "Of_Token";
         when Or_Token => return "Or_Token";
         when Others_Token => return "Others_Token";
         when Out_Token => return "Out_Token";
         when Overriding_Token => return "Overriding_Token";
         when Package_Token => return "Package_Token";
         when Pragma_Token => return "Pragma_Token";
         when Private_Token => return "Private_Token";
         when Procedure_Token => return "Procedure_Token";
         when Protected_Token => return "Protected_Token";
         when Raise_Token => return "Raise_Token";
         when Range_Token => return "Range_Token";
         when Record_Token => return "Record_Token";
         when Rem_Token => return "Rem_Token";
         when Renames_Token => return "Renames_Token";
         when Requeue_Token => return "Requeue_Token";
         when Return_Token => return "Return_Token";
         when Reverse_Token => return "Reverse_Token";
         when Select_Token => return "Select_Token";
         when Separate_Token => return "Separate_Token";
         when Some_Token => return "Some_Token";
         when Subtype_Token => return "Subtype_Token";
         when Synchronized_Token => return "Synchronized_Token";
         when Tagged_Token => return "Tagged_Token";
         when Task_Token => return "Task_Token";
         when Terminate_Token => return "Terminate_Token";
         when Then_Token => return "Then_Token";
         when Type_Token => return "Type_Token";
         when Until_Token => return "Until_Token";
         when Use_Token => return "Use_Token";
         when When_Token => return "When_Token";
         when While_Token => return "While_Token";
         when With_Token => return "With_Token";
         when Xor_Token => return "Xor_Token";
         when others =>
            return Token'Wide_Wide_Image (Value);
      end case;
   end Debug_Image;

   --------------
   -- Get_Next --
   --------------

   overriding function Get_Next (Self : not null access File_Source)
     return Wide_Wide_Character is
   begin
      if Self.Index < Self.Text.Length then
         return Result : constant Wide_Wide_Character :=
           Self.Text.Element (Self.Index).To_Wide_Wide_Character
         do
            Self.Index := Self.Index + 1;
         end return;
      else
         return Incr.Lexers.Batch_Lexers.End_Of_Input;
      end if;
   end Get_Next;

   use type Incr.Ada_Lexers.Token;
   Token       : Incr.Ada_Lexers.Token;
   Source      : aliased File_Source;
   Batch_Lexer : constant Incr.Lexers.Batch_Lexers.Batch_Lexer_Access :=
     new Incr.Ada_Lexers.Batch_Lexer;

begin
   while not Ada.Wide_Wide_Text_IO.End_Of_File loop
      declare
         Line : constant Wide_Wide_String := Ada.Wide_Wide_Text_IO.Get_Line;
      begin
         Source.Text.Append (Line);
         Source.Text.Append (Ada.Characters.Wide_Wide_Latin_1.LF);
      end;
   end loop;

   Batch_Lexer.Set_Source (Source'Unchecked_Access);

   loop
      Batch_Lexer.Get_Token (Token);
      exit when Token = 0;
      Ada.Wide_Wide_Text_IO.Put (Debug_Image (Token));
      Ada.Wide_Wide_Text_IO.Put (' ');
      Ada.Wide_Wide_Text_IO.Put_Line
        (Batch_Lexer.Get_Text.To_Wide_Wide_String);
   end loop;

end Ada_Lexer_Test;
