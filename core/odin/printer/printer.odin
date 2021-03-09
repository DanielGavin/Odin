package odin_printer

import "core:odin/ast"
import "core:odin/tokenizer"
import "core:strings"
import "core:runtime"
import "core:fmt"
import "core:unicode/utf8"
import "core:mem"

Brace_Style :: enum {
	_1TBS,
	Allman
}

Alignment_Style :: enum {
	Align_On_Colon_And_Equals,
	Align_On_Type_And_Equals
}

Config :: struct {
	spaces: int, //Spaces per indentation
	newline_limit: int, //The limit of newlines between statements and declarations.
	tabs: bool, //Enable or disable tabs
	convert_do: bool, //Convert all do statements to brace blocks
	semicolons: bool, //Enable semicolons
	split_multiple_stmts: bool,
	brace_style: Brace_Style,
	align_assignments: bool,
	align_style: Alignment_Style,
	indent_cases: bool
}

default_style := Config {
	spaces = 4,
	newline_limit = 2,
	convert_do = false,
	semicolons = true,
	tabs = true,
	brace_style = ._1TBS,
	split_multiple_stmts = true,
	align_assignments = true,
	align_style = .Align_On_Type_And_Equals,
	indent_cases = false,
};

Printer :: struct {
	string_builder: strings.Builder,
	config: Config,
	source_position: tokenizer.Pos, //the source position from the ast
	out_position: tokenizer.Pos, //the out position of the written data - TODO(daniel, eventually remove the out_position if it never gets used)
	last_position: tokenizer.Pos, //the last position from print function
	whitespaces: []Whitespace, //buffered whitespaces
	current_whitespace: int,
	depth: int, //the identation depth
	comments: [dynamic]^ast.Comment_Group,
	latest_comment_index: int,
	allocator: mem.Allocator,
	skip_semicolon: bool,
	value_decl_aligned_padding: int, //to ensure that assignments and declarations are aligned by name
	value_decl_aligned_type_padding: int, //to ensure that assignments and declarations are aligned by type
	value_decl_aligned_begin_line: int, //the first line part of the aligned calculation
	value_decl_aligned_end_line: int, //the last line part of the aligned calculation
	assign_aligned_padding: int, //ast.Assign_Stmt
	assign_aligned_begin_line: int,
	assign_aligned_end_line: int,
}

Whitespace :: distinct byte;

newline:  Whitespace = '\n';
space:    Whitespace = ' ';
unindent: Whitespace = '<';
indent:   Whitespace = '>';
ignore:   Whitespace = '!';

lbrace:    byte = '{';
rbrace:    byte = '}';
lparen:    byte = '(';
rparen:    byte = ')';
semicolon: byte = ';';
lbracket:  byte = '[';
rbracket:  byte = ']';
comma:     byte = ',';
dot:       byte = '.';

make_printer :: proc (config: Config, allocator := context.allocator) -> Printer {
	return {
		string_builder = strings.make_builder(allocator),
		config = config,
		source_position = {
			line = 1,
			column = 1
		},
		out_position = {
			line = 1,
			column = 1
		},
		allocator = allocator,
		whitespaces = make([]Whitespace, 100, allocator)
	};
}

to_string :: proc (printer: Printer) -> string {
	return strings.to_string(printer.string_builder);
}

print :: proc (p: ^Printer, args: ..any) {

	for arg in args {

		data: string;
		b:    byte;
		tok:  tokenizer.Token;

		switch a in arg{
		case string:
			data = a;
		case tokenizer.Token:
			tok = a;
			data = a.text;
		case byte:
			b = a;
			data = strings.string_from_ptr(&b, 1);
		case Whitespace:

			//consume the whitespaces if it every reaches buffer size
			if p.current_whitespace == len(p.whitespaces) {
				write_whitespaces(p, p.current_whitespace);
			}

			p.whitespaces[p.current_whitespace] = a;
			p.current_whitespace += 1;
			continue;
		case ast.Ident:
			data = a.name;
		case:
			panic("unsupported argument");
		}

		next := p.source_position;

		if comment_before_position(p, next) {
			write_comments(p, next);
		}

		else {
			write_whitespaces(p, p.current_whitespace);
		}

		write_string(p, next, data);
	}
}

newline_until_pos :: proc (p: ^Printer, pos: tokenizer.Pos) {
	newline_until_pos_limit(p, pos, p.config.newline_limit);
}

newline_until_pos_limit :: proc (p: ^Printer, pos: tokenizer.Pos, limit: int) {

	lines := min(pos.line - p.source_position.line, limit);

	for i := 0; i < lines; i += 1{
		print(p, newline);
	}

	//all whitespaces are buffered and we therefore set the expected source_position
	p.source_position = pos;
}

comment_before_position :: proc (p: ^Printer, pos: tokenizer.Pos) -> bool {

	if len(p.comments) <= p.latest_comment_index {
		return false;
	}

	comment := p.comments[p.latest_comment_index];

	return comment.pos.offset < pos.offset;
}

next_comment_group :: proc (p: ^Printer) {
	p.latest_comment_index += 1;
}

prefix_comment_group :: proc (p: ^Printer, last: tokenizer.Pos, pos: tokenizer.Pos, comment_group: ^ast.Comment_Group) -> int {

	//unwind all the newlines from the last position of a non whitespace to the current

	indent_change := 0;

	for i := 0; i < p.current_whitespace; i += 1{

		switch w := p.whitespaces[i]; w{
		case newline:
			p.whitespaces[i] = ignore;
		case unindent:
			p.whitespaces[i] = ignore;
			indent_change -= 1;
		}
	}

	write_whitespaces(p, p.current_whitespace);

	return indent_change;
}

postfix_comments :: proc (p: ^Printer, pos: tokenizer.Pos, last_comment: ^tokenizer.Token) {

	//after writing all the comments we need to make sure that the token we are writing is newlined correctly

	//account for multiline comments
	newlines := strings.count(last_comment.text, "\n");

	lines := min(pos.line - last_comment.pos.line - newlines, p.config.newline_limit);

	for i := 0; i < lines; i += 1{
		write_byte(p, '\n');
	}
}

write_prefix_comment :: proc (p: ^Printer, prev_comment: ^tokenizer.Token, comment: tokenizer.Token) {

	//handle the placement of the comment with newlines

	if p.last_position.line == comment.pos.line {
		write_byte(p, cast(byte)space);
	}

	else {

		lines: int;

		if prev_comment != nil {
			newlines := strings.count(prev_comment.text, "\n");
			lines = comment.pos.line - prev_comment.pos.line - newlines;
		}

		else {
			lines = comment.pos.line - p.last_position.line;
		}

		lines = min(p.config.newline_limit, lines);

		for i := 0; i < lines; i += 1{
			write_byte(p, '\n');
		}
	}
}

write_comment :: proc (p: ^Printer, comment: tokenizer.Token) {

	if len(comment.text) == 0 {
		return;
	}

	if comment.text[0] == '/' && comment.text[1] == '/' {
		write_string(p, comment.pos, comment.text);
	}

	else {

		c_len := len(comment.text);

		trim_space := true;

		for i := 0; i < len(comment.text); i += 1{

			c := comment.text[i];

			if c != ' ' && c != '\t' {
				trim_space = false;
			}

			if (c == ' ' || c == '\t') && trim_space {
				continue;
			}

			else if c == 13 && comment.text[min(c_len - 1, i + 1)] == 10 {
				write_byte(p, '\n');
				trim_space = true;
				i += 1;
			}

			else if c == '/' && comment.text[min(c_len - 1, i + 1)] == '*' {
				write_string(p, p.source_position, "/*");
				trim_space = true;
				p.depth += 1;
				i += 1;
			}

			else if c == '*' && comment.text[min(c_len - 1, i + 1)] == '/' {
				p.depth -= 1;
				trim_space = true;
				write_string(p, p.source_position, "*/");
				i += 1;
			}

			else {
				write_byte(p, c);
			}
		}
	}
}

write_comments :: proc (p: ^Printer, pos: tokenizer.Pos) {

	next := p.last_position;
	prev_comment: ^tokenizer.Token;

	for comment_before_position(p, pos) {

		comment_group := p.comments[p.latest_comment_index];

		indentation := prefix_comment_group(p, next, pos, comment_group);

		for comment, i in comment_group.list {
			write_prefix_comment(p, prev_comment, comment);
			write_comment(p, comment);
			next = comment.pos;
			prev_comment = &comment_group.list[i];
		}

		//restore the indentation
		p.depth += indentation;

		next_comment_group(p);
	}

	if prev_comment != nil {
		postfix_comments(p, pos, prev_comment);
	}
}

write_whitespaces :: proc (p: ^Printer, n: int) {

	for i := 0; i < n; i += 1{

		switch c := p.whitespaces[i]; c{
		case ignore:
			continue;
		case indent:
			p.depth += 1;
		case unindent:
			p.depth -= 1;
		case newline:
			fallthrough;
		case:
			write_byte(p, cast(byte)p.whitespaces[i]);
		}
	}

	l := copy(p.whitespaces, p.whitespaces[n:p.current_whitespace]);

	p.current_whitespace = l;
}

write_string :: proc (p: ^Printer, pos: tokenizer.Pos, str: string) {

	bytes := len(str);
	runes := strings.rune_count(str);

	if p.out_position.column == 1 {
		write_indent(p);
	}

	p.source_position = pos;
	p.source_position.offset += bytes;
	p.source_position.column += runes;
	p.out_position.offset += bytes;
	p.out_position.column += runes;
	strings.write_string(&p.string_builder, str);

	p.last_position = p.source_position;
}

write_byte :: proc (p: ^Printer, b: byte) {

	if p.out_position.column == 1 && b != '\t' && b != ' ' && b != '\n' {
		write_indent(p);
	}

	if b == '\n' {
		p.source_position.column = 1;
		p.source_position.line += 1;
		p.source_position.offset += 1;
		p.out_position.column = 1;
		p.out_position.line += 1;
		p.out_position.offset += 1;
	}

	else {
		p.source_position.offset += 1;
		p.out_position.offset += 1;
		p.source_position.column += 1;
		p.out_position.column += 1;
	}

	strings.write_byte(&p.string_builder, b);
}

write_indent :: proc (p: ^Printer) {

	if p.config.tabs {
		for i := 0; i < p.depth; i += 1{
			write_byte(p, '\t');
		}
	}

	else {
		for i := 0; i < p.depth * p.config.spaces; i += 1{
			write_byte(p, ' ');
		}
	}
}

set_source_position :: proc (p: ^Printer, pos: tokenizer.Pos) {
	p.source_position = pos;
}

print_expr :: proc (p: ^Printer, expr: ^ast.Expr) {

	using ast;

	if expr == nil {
		return;
	}

	set_source_position(p, expr.pos);

	switch v in expr.derived{
	case Ellipsis:
		print(p, "..");
		print_expr(p, v.expr);
	case Relative_Type:
		print_expr(p, v.tag);
		print(p, space);
		print_expr(p, v.type);
	case Slice_Expr:
		print_expr(p, v.expr);
		print(p, lbracket);
		print_expr(p, v.low);
		print(p, v.interval);
		print_expr(p, v.high);
		print(p, rbracket);
	case Ident:
		print(p, v);
	case Deref_Expr:
		print_expr(p, v.expr);
		print(p, v.op);
	case Type_Cast:
		print(p, v.tok, lparen);
		print_expr(p, v.type);
		print(p, rparen);
		print_expr(p, v.expr);
	case Basic_Directive:
		print(p, v.tok, v.name);
	case Distinct_Type:
		print(p, "distinct", space);
		print_expr(p, v.type);
	case Dynamic_Array_Type:
		print_expr(p, v.tag);
		print(p, lbracket, "dynamic", rbracket);
		print_expr(p, v.elem);
	case Bit_Set_Type:
		print(p, "bit_set", lbracket);
		print_expr(p, v.elem);

		if v.underlying != nil {
			print(p, semicolon, space);
			print_expr(p, v.underlying);
		}

		print(p, rbracket);
	case Union_Type:
		print(p, "union");

		if v.poly_params != nil {
			print(p, lparen);
			print_field_list(p, v.poly_params, ", ");
			print(p, rparen);
		}

		if v.is_maybe {
			print(p, space, "#maybe");
		}

		set_source_position(p, v.variants[len(v.variants) - 1].pos);

		if v.variants != nil && (len(v.variants) == 0 || v.pos.line == v.end.line) {
			print(p, space, lbrace);
			print_exprs(p, v.variants, ", ");
			print(p, rbrace);
		}

		else {
			print(p, space);
			print_begin_brace(p);
			print(p, newline);
			print_exprs(p, v.variants, ",", true);
			print_end_brace(p);
		}
	case Enum_Type:
		print(p, "enum");

		if v.base_type != nil {
			print(p, space);
			print_expr(p, v.base_type);
		}

		set_source_position(p, v.fields[len(v.fields) - 1].pos);

		if v.fields != nil && (len(v.fields) == 0 || v.pos.line == v.end.line) {
			print(p, space, lbrace);
			print_exprs(p, v.fields, ", ");
			print(p, rbrace);
		}

		else {
			print(p, space);
			print_begin_brace(p);
			print(p, newline);
			print_enum_fields(p, v.fields, ",");
			print_end_brace(p);
		}

		set_source_position(p, v.end);
	case Struct_Type:
		print(p, "struct");

		if v.is_packed {
			print(p, space, "#packed");
		}

		if v.is_raw_union {
			print(p, space, "#raw_union");
		}

		if v.align != nil {
			print(p, space, "#align", space);
			print_expr(p, v.align);
		}

		if v.poly_params != nil {
			print(p, lparen);
			print_field_list(p, v.poly_params, ", ");
			print(p, rparen);
		}

		set_source_position(p, v.fields.pos);

		if v.fields != nil && (len(v.fields.list) == 0 || v.pos.line == v.end.line) {
			print(p, space, lbrace);
			print_field_list(p, v.fields, ", ");
			print(p, rbrace);
		}

		else {
			print(p, space);
			print_begin_brace(p);
			print(p, newline);
			print_struct_field_list(p, v.fields, ",");
			set_source_position(p, v.end); //should really be the source position of the brace(no token)
			print_end_brace(p);
		}

		set_source_position(p, v.end);
	case Proc_Lit:

		if v.inlining == .Inline {
			print(p, "#force_inline", space);
		}

		print_proc_type(p, v.type^);

		if v.where_clauses != nil {
			print(p, space);
			newline_until_pos(p, v.where_clauses[0].pos);
			print(p, "where", space);
			print_exprs(p, v.where_clauses, ", ");
		}

		if v.body != nil {
			print(p, space);
			print_stmt(p, v.body);
		}

		else {
			print(p, space, "---");
		}
	case Proc_Type:
		print_proc_type(p, v);
	case Basic_Lit:
		print(p, v.tok);
	case Binary_Expr:
		print_binary_expr(p, v);
	case Implicit_Selector_Expr:
		print(p, dot, v.field^);
	case Call_Expr:
		print_expr(p, v.expr);
		print(p, lparen);
		print_exprs(p, v.args, ", ");
		print(p, rparen);
	case Typeid_Type:
		print(p, "typeid");
		if v.specialization != nil {
			print(p, "/");
			print_expr(p, v.specialization);
		}
	case Selector_Expr:
		print_expr(p, v.expr);
		print(p, dot);
		print_expr(p, v.field);
	case Paren_Expr:
		print(p, lparen);
		print_expr(p, v.expr);
		print(p, rparen);
	case Index_Expr:
		print_expr(p, v.expr);
		print(p, lbracket);
		print_expr(p, v.index);
		print(p, rbracket);
	case Proc_Group:
		print(p, v.tok, space, lbrace);
		print_exprs(p, v.args, ", ");
		print(p, rbrace);
	case Comp_Lit:

		if v.type != nil {
			print_expr(p, v.type);
			print(p, space);
		}

		if len(v.elems) != 0 {
			set_source_position(p, v.elems[0].pos);
		}

		if len(v.elems) != 0 && v.pos.line != v.elems[len(v.elems) - 1].pos.line {
			print_begin_brace(p);
			print(p, newline);
			print_exprs(p, v.elems, ", ");
			print_end_brace(p);
		}

		else {
			print(p, lbrace);
			print_exprs(p, v.elems, ", ");
			print(p, rbrace);
		}
	case Unary_Expr:
		print(p, v.op);
		print_expr(p, v.expr);
	case Field_Value:
		print_expr(p, v.field);
		print(p, space, "=", space);
		print_expr(p, v.value);
	case Type_Assertion:
		print_expr(p, v.expr);

		if unary, ok := v.type.derived.(Unary_Expr); ok && unary.op.text == "?" {
			print(p, dot);
			print_expr(p, v.type);
		}

		else {
			print(p, dot, lparen);
			print_expr(p, v.type);
			print(p, rparen);
		}

	case Pointer_Type:
		print(p, "^");
		print_expr(p, v.elem);
	case Implicit:
		print(p, v.tok);
	case Poly_Type:
		print(p, "$");
		print_expr(p, v.type);

		if v.specialization != nil {
			print(p, "/");
			print_expr(p, v.specialization);
		}
	case Array_Type:
		print_expr(p, v.tag);
		print(p, lbracket);
		print_expr(p, v.len);
		print(p, rbracket);
		print_expr(p, v.elem);
	case Map_Type:
		print(p, "map", lbracket);
		print_expr(p, v.key);
		print(p, rbracket);
		print_expr(p, v.value);
	case Helper_Type:
		print_expr(p, v.type);
	case:
		panic(fmt.aprint(expr.derived));
	}
}

print_proc_type :: proc (p: ^Printer, proc_type: ast.Proc_Type) {

	print(p, "proc"); //TOOD(ast is missing proc token)

	if proc_type.calling_convention != .None {
		print(p, space);
	}

	switch proc_type.calling_convention{
	case .Odin:
	case .Contextless:
		print(p, "\"contextless\"", space);
	case .C_Decl:
		print(p, "\"c\"", space);
	case .Std_Call:
		print(p, "\"std\"", space);
	case .Fast_Call:
		print(p, "\"fast\"", space);
	case .None:
		//nothing i guess
	case .Invalid:
		//nothing i guess
	case .Foreign_Block_Default:
	}

	print(p, lparen);
	print_signature_list(p, proc_type.params, ", ");
	print(p, rparen);

	if proc_type.results != nil {
		print(p, space, "->", space);

		use_parens := false;
		use_named  := false;

		if len(proc_type.results.list) > 1 {
			use_parens = true;
		}

		else if len(proc_type.results.list) == 1 {

			for name in proc_type.results.list[0].names {
				if ident, ok := name.derived.(ast.Ident); ok {
					if ident.name != "_" {
						use_parens = true;
					}
				}
			}
		}

		if use_parens {
			print(p, lparen);
			print_signature_list(p, proc_type.results, ", ");
			print(p, rparen);
		}

		else {
			print_signature_list(p, proc_type.results, ", ");
		}
	}
}

print_enum_fields :: proc (p: ^Printer, list: []^ast.Expr, sep := " ") {

	//print enum fields is like print_exprs, but it can contain fields that can be aligned.

	if len(list) == 0 {
		return;
	}

	if list[0].pos.line == list[len(list) - 1].pos.line {
		//if everything is on one line, then it can be treated the same way as print_exprs
		print_exprs(p, list, sep);
		return;
	}

	largest          := 0;
	last_field_value := 0;

	//first find all the field values and find the largest name
	for expr, i in list {

		if field_value, ok := expr.derived.(ast.Field_Value); ok {

			if ident, ok := field_value.field.derived.(ast.Ident); ok {
				largest = max(largest, strings.rune_count(ident.name));
			}
		}
	}

	for expr, i in list {

		newline_until_pos_limit(p, expr.pos, 1);

		if field_value, ok := expr.derived.(ast.Field_Value); ok && p.config.align_assignments {

			if ident, ok := field_value.field.derived.(ast.Ident); ok {
				print_expr(p, field_value.field);
				print_space_padding(p, largest - strings.rune_count(ident.name) + 1);
				print(p, "=", space);
				print_expr(p, field_value.value);
			}

			else {
				print_expr(p, expr);
			}
		}

		else {
			print_expr(p, expr);
		}

		if i != len(list) - 1 {
			print(p, sep);
		}

		else {
			print(p, strings.trim_space(sep));
		}
	}
}

print_exprs :: proc (p: ^Printer, list: []^ast.Expr, sep := " ", trailing := false) {

	if len(list) == 0 {
		return;
	}

	//all the expression are on the line
	if list[0].pos.line == list[len(list) - 1].pos.line {

		for expr, i in list {

			print_expr(p, expr);

			if i != len(list) - 1 {
				print(p, sep);
			}
		}
	}

	else

	//we have to newline the expressions to respect the source
	{

		for expr, i in list {

			newline_until_pos_limit(p, expr.pos, 1);

			print_expr(p, expr);

			if i != len(list) - 1 {
				print(p, sep);
			}

			else if trailing {
				print(p, strings.trim_space(sep));
			}
		}
	}
}

print_binary_expr :: proc (p: ^Printer, binary: ast.Binary_Expr) {

	if v, ok := binary.left.derived.(ast.Binary_Expr); ok {
		print_binary_expr(p, v);
	}

	else {
		print_expr(p, binary.left);
	}

	if binary.op.kind == .Ellipsis || binary.op.kind == .Range_Half {
		print(p, binary.op);
	}

	else {
		print(p, space, binary.op, space);
	}

	newline_until_pos(p, binary.right.pos);

	if v, ok := binary.right.derived.(ast.Binary_Expr); ok {
		print_binary_expr(p, v);
	}

	else {
		print_expr(p, binary.right);
	}
}

print_struct_field_list :: proc(p: ^Printer, list: ^ast.Field_List, sep := "") {

	if list.list == nil {
		return;
	}

	largest := 0;

	for field, i in list.list {
		largest = max(largest, get_length_of_names(field.names));
	}

	for field, i in list.list {

		newline_until_pos_limit(p, field.pos, 1);

		if .Using in field.flags {
			print(p, "using", space);
		}

		print_exprs(p, field.names, ", ");

		if len(field.names) != 0 {
			print(p, ": ");
		}

		if field.type == nil {
			panic("struct field has to have types");
		}

		print_space_padding(p, largest - get_length_of_names(field.names));

		print_expr(p, field.type);

		if field.tag.text != "" {
			print(p, space, field.tag);
		}

		if i != len(list.list) - 1 {
			print(p, sep);
		}

		else{
			print(p, strings.trim_space(sep));
		}
	}

}

print_field_list :: proc (p: ^Printer, list: ^ast.Field_List, sep := "") {

	if list.list == nil {
		return;
	}

	for field, i in list.list {

		newline_until_pos_limit(p, field.pos, 1);

		if .Using in field.flags {
			print(p, "using", space);
		}

		print_exprs(p, field.names, ", ");

		if len(field.names) != 0 {
			print(p, ": ");
		}

		if field.type != nil {
			print_expr(p, field.type);
		}

		else {
			print(p, ":= ");
			print_expr(p, field.default_value);
		}

		if field.tag.text != "" {
			print(p, space, field.tag);
		}

		if i != len(list.list) - 1 {
			print(p, sep);
		}
	}
}

print_signature_list :: proc (p: ^Printer, list: ^ast.Field_List, sep := "") {

	if list.list == nil {
		return;
	}

	for field, i in list.list {

		newline_until_pos_limit(p, field.pos, 1);

		if .Using in field.flags {
			print(p, "using", space);
		}

		named := false;

		for name in field.names {
			if ident, ok := name.derived.(ast.Ident); ok {
				//for some reason the parser uses _ to mean empty
				if ident.name != "_" {
					named = true;
				}
			}

			else {
				//alternative is poly names
				named = true;
			}
		}

		if named {
			print_exprs(p, field.names, ", ");

			if len(field.names) != 0 && field.default_value == nil {
				print(p, ": ");
			}

			else {
				print(p, space);
			}
		}

		if field.type != nil {
			print_expr(p, field.type);
		}

		else {
			print(p, ":= ");
			print_expr(p, field.default_value);
		}

		if i != len(list.list) - 1 {
			print(p, sep);
		}
	}
}

print_stmt :: proc (p: ^Printer, stmt: ^ast.Stmt, empty_block := false, block_stmt := false) {

	using ast;

	if stmt == nil {
		return;
	}

	switch v in stmt.derived{
	case Value_Decl:
		print_decl(p, cast(^Decl)stmt, true);
		return;
	case Foreign_Import_Decl:
		print_decl(p, cast(^Decl)stmt, true);
		return;
	case Foreign_Block_Decl:
		print_decl(p, cast(^Decl)stmt, true);
		return;
	}

	switch v in stmt.derived{
	case Using_Stmt:
		newline_until_pos(p, v.pos);
		print(p, "using", space);
		print_exprs(p, v.list, ", ");
		print(p, semicolon);
	case Block_Stmt:
		newline_until_pos(p, v.pos);

		if v.pos.line == v.end.line && len(v.stmts) > 1 && p.config.split_multiple_stmts {

			if !empty_block {
				print_begin_brace(p);
			}

			set_source_position(p, v.pos);

			print_block_stmts(p, v.stmts, true);

			set_source_position(p, v.end);

			if !empty_block {
				print_end_brace(p);
			}
		}

		else if v.pos.line == v.end.line {
			if !empty_block {
				print(p, lbrace);
			}

			set_source_position(p, v.pos);

			print_block_stmts(p, v.stmts);

			set_source_position(p, v.end);

			if !empty_block {
				print(p, rbrace);
			}
		}

		else {
			if !empty_block {
				print_begin_brace(p);
			}

			set_source_position(p, v.pos);

			print_block_stmts(p, v.stmts);

			set_source_position(p, v.end);

			if !empty_block {
				print_end_brace(p);
			}
		}
	case If_Stmt:
		newline_until_pos(p, v.pos);

		if v.label != nil {
			print_expr(p, v.label);
			print(p, ":", space);
		}

		print(p, "if", space);

		if v.init != nil {
			p.skip_semicolon = true;
			print_stmt(p, v.init);
			p.skip_semicolon = false;
			print(p, semicolon, space);
		}

		print_expr(p, v.cond);

		uses_do := false;

		if check_stmt, ok := v.body.derived.(Block_Stmt); ok && check_stmt.uses_do {
			uses_do = true;
		}

		if uses_do && !p.config.convert_do {
			print(p, space, "do", space);
			print_stmt(p, v.body, true);
		}

		else {
			if uses_do {
				print(p, newline);
			}

			print(p, space);

			print_stmt(p, v.body);
		}

		if v.else_stmt != nil {
			print(p, newline, newline, "else");
			print(p, space);

			set_source_position(p, v.else_stmt.pos);

			print_stmt(p, v.else_stmt);
		}
	case Switch_Stmt:
		newline_until_pos(p, v.pos);

		if v.label != nil {
			print_expr(p, v.label);
			print(p, ":", space);
		}

		if v.partial {
			print(p, "#partial", space);
		}

		print(p, "switch", space);

		if v.init != nil {
			p.skip_semicolon = true;
			print_stmt(p, v.init);
			p.skip_semicolon = false;
			print(p, semicolon, space);
		}

		print_expr(p, v.cond);
		print(p, space);
		print_stmt(p, v.body);
	case Case_Clause:
		newline_until_pos(p, v.pos);

		if !p.config.indent_cases {
			print(p, unindent);
		}

		print(p, "case", indent);

		if v.list != nil {
			print(p, space);
			print_exprs(p, v.list, ",");
		}

		print(p, v.terminator);

		for stmt in v.body {
			print_stmt(p, stmt, false, true);
		}

		print(p, unindent);

		if !p.config.indent_cases {
			print(p, indent);
		}
	case Type_Switch_Stmt:
		newline_until_pos(p, v.pos);

		if v.label != nil {
			print_expr(p, v.label);
			print(p, ":", space);
		}

		if v.partial {
			print(p, "#partial", space);
		}

		print(p, "switch", space);

		print_stmt(p, v.tag);
		print(p, space);
		print_stmt(p, v.body);
	case Assign_Stmt:
		newline_until_pos(p, v.pos);

		/*
			if len(v.lhs) == 1 {

			if ident, ok := v.lhs[0].derived.(Ident); ok && ident.name == "_" {
			print(p, v.op, space);
			print_exprs(p, v.rhs, ", ");
			return;
			}

			}
		*/

		print_exprs(p, v.lhs, ", ");

		if p.config.align_assignments && p.assign_aligned_begin_line <= v.pos.line && v.pos.line <= p.assign_aligned_end_line {
			print_space_padding(p, p.assign_aligned_padding - get_length_of_names(v.lhs));
		}

		print(p, space, v.op, space);

		print_exprs(p, v.rhs, ", ");

		if block_stmt && p.config.semicolons {
			print(p, semicolon);
		}
	case Expr_Stmt:
		newline_until_pos(p, v.pos);
		print_expr(p, v.expr);
		if block_stmt {
			print(p, semicolon);
		}
	case For_Stmt:

		newline_until_pos(p, v.pos);

		if v.label != nil {
			print_expr(p, v.label);
			print(p, ":", space);
		}

		print(p, "for");

		if v.init != nil || v.cond != nil || v.post != nil {
			print(p, space);
		}

		if v.init != nil {
			p.skip_semicolon = true;
			print_stmt(p, v.init);
			p.skip_semicolon = false;
			print(p, semicolon, space);
		}

		if v.cond != nil {
			print_expr(p, v.cond);
		}

		if v.init != nil {
			print(p, semicolon);
		}

		if v.post != nil {
			print(p, space);
			print_stmt(p, v.post);
		}

		if v.init == nil && v.post == nil && v.cond != nil {
			print(p, space);
		}

		print_stmt(p, v.body);
	case Inline_Range_Stmt:

		newline_until_pos(p, v.pos);

		if v.label != nil {
			print_expr(p, v.label);
			print(p, ":", space);
		}

		print(p, "#unroll", space);

		print(p, "for", space);
		print_expr(p, v.val0);

		if v.val1 != nil {
			print(p, ",", space);
			print_expr(p, v.val1);
			print(p, space);
		}

		print(p, "in", space);
		print_expr(p, v.expr);
		print(p, space);

		print_stmt(p, v.body);
	case Range_Stmt:

		newline_until_pos(p, v.pos);

		if v.label != nil {
			print_expr(p, v.label);
			print(p, ":", space);
		}

		print(p, "for", space);
		print_expr(p, v.val0);

		if v.val1 != nil {
			print(p, ",", space);
			print_expr(p, v.val1);
			print(p, space);
		}

		else {
			print(p, space);
		}

		print(p, "in", space);
		print_expr(p, v.expr);
		print(p, space);

		print_stmt(p, v.body);
	case Return_Stmt:
		newline_until_pos(p, v.pos);
		print(p, "return");

		if v.results != nil {
			print(p, space);
			print_exprs(p, v.results, ", ");
		}

		if block_stmt && p.config.semicolons {
			print(p, semicolon);
		}
	case Defer_Stmt:
		newline_until_pos(p, v.pos);
		print(p, "defer", space);
		print_stmt(p, v.stmt);
		print(p, semicolon);
	case When_Stmt:
		newline_until_pos(p, v.pos);
		print(p, "when", space);
		print_expr(p, v.cond);
		print(p, space);

		print_stmt(p, v.body);

		if v.else_stmt != nil {

			print(p, newline, newline, "else");
			print(p, space);

			print_stmt(p, v.else_stmt);
		}

	case Branch_Stmt:

		newline_until_pos(p, v.pos);

		print(p, v.tok);

		if v.label != nil {
			print(p, space);
			print_expr(p, v.label);
		}

		if p.config.semicolons {
			print(p, semicolon);
		}
	case:
		panic(fmt.aprint(stmt.derived));
	}

	set_source_position(p, stmt.end);
}

print_decl :: proc (p: ^Printer, decl: ^ast.Decl, called_in_stmt := false) {

	using ast;

	if decl == nil {
		return;
	}

	switch v in decl.derived{
	case When_Stmt:
		print_stmt(p, cast(^Stmt)decl);
	case Foreign_Import_Decl:
		if len(v.attributes) > 0 {
			newline_until_pos(p, v.attributes[0].pos);
		}

		else {
			newline_until_pos(p, decl.pos);
		}

		print_attributes(p, v.attributes);
		print(p, v.foreign_tok, space, v.import_tok, space, v.name^, space);

		for path in v.fullpaths {
			print(p, path);
		}
	case Foreign_Block_Decl:

		if len(v.attributes) > 0 {
			newline_until_pos(p, v.attributes[0].pos);
		}

		else {
			newline_until_pos(p, decl.pos);
		}

		print_attributes(p, v.attributes);

		print(p, newline, "foreign", space);
		print_expr(p, v.foreign_library);
		print_stmt(p, v.body);
	case Import_Decl:
		newline_until_pos(p, decl.pos);

		if v.name.text != "" {
			print(p, v.import_tok, " ", v.name, " ", v.fullpath);
		}

		else {
			print(p, v.import_tok, " ", v.fullpath);
		}

	case Value_Decl:
		if len(v.attributes) > 0 {
			newline_until_pos(p, v.attributes[0].pos);
			print_attributes(p, v.attributes);
		}

		newline_until_pos(p, decl.pos);

		if v.is_using {
			print(p, "using", space);
		}

		print_exprs(p, v.names, ", ");

		seperator := ":";

		if !v.is_mutable && v.type == nil {
			seperator = ":: ";
		}

		else if !v.is_mutable && v.type != nil {
			seperator = " :";
		}

		if in_value_decl_alignment(p, v) && p.config.align_style == .Align_On_Colon_And_Equals {
			print_space_padding(p, p.value_decl_aligned_padding - get_length_of_names(v.names));
		}

		if v.type != nil {
			print(p, seperator, space);

			if in_value_decl_alignment(p, v) && p.config.align_style == .Align_On_Type_And_Equals {
				print_space_padding(p, p.value_decl_aligned_padding - get_length_of_names(v.names));
			}

			else if in_value_decl_alignment(p, v) && p.config.align_style == .Align_On_Colon_And_Equals {
				print_space_padding(p, p.value_decl_aligned_type_padding - (v.type.end.column - v.type.pos.column));
			}

			print_expr(p, v.type);

			if in_value_decl_alignment(p, v) && p.config.align_style == .Align_On_Type_And_Equals && len(v.values) != 0 {
				print_space_padding(p, p.value_decl_aligned_type_padding - (v.type.end.column - v.type.pos.column));
			}
		}

		else {
			if in_value_decl_alignment(p, v) && p.config.align_style == .Align_On_Type_And_Equals {
				print_space_padding(p, p.value_decl_aligned_padding - get_length_of_names(v.names));
			}
			print(p, space, seperator);
		}

		if v.is_mutable && v.type != nil && len(v.values) != 0 {
			print(p, space, "=", space);
		}

		else if v.is_mutable && v.type == nil && len(v.values) != 0 {
			print(p, "=", space);
		}

		else if !v.is_mutable && v.type != nil {
			print(p, space, ":", space);
		}

		print_exprs(p, v.values, ", ");

		add_semicolon := true;

		for value in v.values {
			switch a in value.derived{
			case Proc_Lit,Union_Type,Enum_Type,Struct_Type:
				add_semicolon = false || called_in_stmt;
			}
		}

		if add_semicolon && p.config.semicolons && !p.skip_semicolon {
			print(p, semicolon);
		}

	case:
		panic(fmt.aprint(decl.derived));
	}
}

print_attributes :: proc (p: ^Printer, attributes: [dynamic]^ast.Attribute) {

	if len(attributes) == 0 {
		return;
	}

	for attribute, i in attributes {

		print(p, "@", lparen);
		print_exprs(p, attribute.elems, ", ");
		print(p, rparen);

		if len(attributes) - 1 != i {
			print(p, newline);
		}
	}
}

print_file :: proc (p: ^Printer, file: ^ast.File) {

	p.comments = file.comments;

	print(p, file.pkg_token, space, file.pkg_name);

	for decl, i in file.decls {

		if value_decl, ok := decl.derived.(ast.Value_Decl); ok {
			set_value_decl_alignment_padding(p, value_decl, file.decls[i + 1:]);
		}

		print_decl(p, cast(^ast.Decl)decl);
	}
}

print_begin_brace :: proc (p: ^Printer) {

	if p.config.brace_style == .Allman {

		//only newline when it isn't a empty block
		if p.last_position.line == p.source_position.line {
			print(p, newline);
		}

		print(p, lbrace);
		print(p, indent);
	}

	else if p.config.brace_style == ._1TBS {
		print(p, lbrace);
		print(p, indent);
	}
}

print_end_brace :: proc (p: ^Printer) {
	print(p, unindent);
	print(p, newline, rbrace);
}

print_block_stmts :: proc (p: ^Printer, stmts: []^ast.Stmt, newline_each := false) {
	for stmt, i in stmts {

		if newline_each {
			print(p, newline);
		}

		if value_decl, ok := stmt.derived.(ast.Value_Decl); ok {
			set_value_decl_alignment_padding(p, value_decl, stmts[i + 1:]);
		}

		else if assignment_stmt, ok := stmt.derived.(ast.Assign_Stmt); ok {
			set_assign_alignment_padding(p, assignment_stmt, stmts[i + 1:]);
		}

		print_stmt(p, stmt, false, true);
	}
}

print_space_padding :: proc (p: ^Printer, n: int) {

	for i := 0; i < n; i += 1{
		print(p, space);
	}
}

set_value_decl_alignment_padding :: proc (p: ^Printer, value_decl: ast.Value_Decl, stmts: []^ast.Stmt) {

	if p.value_decl_aligned_begin_line <= value_decl.pos.line && value_decl.pos.line <= p.value_decl_aligned_end_line {
		//we have already calculated it for this line
		return;
	}

	largest_name := get_length_of_names(value_decl.names);
	last_line    := value_decl.pos.line;
	p.value_decl_aligned_begin_line = last_line;

	largest_type := 0;

	if value_decl.type != nil {
		largest_type = value_decl.type.end.column - value_decl.type.pos.column;
	}

	for stmt in stmts {

		if next_decl, ok := stmt.derived.(ast.Value_Decl); ok {

			if last_line + 1 != next_decl.pos.line || value_decl.is_mutable != next_decl.is_mutable {
				break;
			}

			if value_decl.type == nil && next_decl.type != nil {
				break;
			}

			if value_decl.type != nil && next_decl.type == nil {
				break;
			}

			largest_name = max(largest_name, get_length_of_names(next_decl.names));

			if next_decl.type != nil {
				largest_type = max(largest_type, next_decl.type.end.column - next_decl.type.pos.column);
			}

			last_line = stmt.pos.line;
		}

		else {
			break;
		}
	}

	p.value_decl_aligned_end_line = last_line;
	p.value_decl_aligned_padding = largest_name;
	p.value_decl_aligned_type_padding = largest_type;
}

set_assign_alignment_padding :: proc (p: ^Printer, assign: ast.Assign_Stmt, stmts: []^ast.Stmt) {

	if p.assign_aligned_begin_line <= assign.pos.line && assign.pos.line <= p.assign_aligned_end_line {
		//we have already calculated it for this line
		return;
	}

	largest_name := get_length_of_names(assign.lhs);
	last_line    := assign.pos.line;
	p.assign_aligned_begin_line = last_line;

	for stmt in stmts {

		if next_assign, ok := stmt.derived.(ast.Assign_Stmt); ok {

			if last_line + 1 != next_assign.pos.line || len(assign.lhs) != len(next_assign.lhs) {
				break;
			}

			largest_name = max(largest_name, get_length_of_names(next_assign.lhs));
			last_line = stmt.pos.line;
		}

		else {
			break;
		}
	}

	p.assign_aligned_end_line = last_line;
	p.assign_aligned_padding = largest_name;
}

get_length_of_names :: proc (names: []^ast.Expr) -> int {
	sum := 0;

	for name, i in names {
		ident := name.derived.(ast.Ident);
		sum += strings.rune_count(ident.name);

		if i != len(names) - 1 {
			sum += 2; //space and comma
		}
	}

	return sum;
}

in_value_decl_alignment :: proc (p: ^Printer, v: ast.Value_Decl) -> bool {
	return p.config.align_assignments && p.value_decl_aligned_begin_line <= v.pos.line && v.pos.line <= p.value_decl_aligned_end_line;
}