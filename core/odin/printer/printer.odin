package odin_printer

import "core:odin/ast"
import "core:odin/tokenizer"
import "core:strings"
import "core:runtime"
import "core:fmt"
import "core:unicode/utf8"
import "core:mem"

Bracket_Style :: enum {
	_1TBS,
	Allman,
}

Config :: struct {
	spaces: int,
	newline_limit: int,
	tabs: bool,
	convert_do: bool,
	semicolons: bool,
	bracket_style: Bracket_Style,
}

default_style := Config {
	spaces = 4,
	newline_limit = 2,
	convert_do = false,
	semicolons = false,
	bracket_style = .Allman,
};


Printer :: struct {
	string_builder: strings.Builder,
	config: Config,
	source_position: tokenizer.Pos, //the source position from the ast
	out_position: tokenizer.Pos, //the out position of the written data
	last_position: tokenizer.Pos, //the last position from print function
	whitespaces: [] Whitespace, //buffered whitespaces
	current_whitespace: int,
	depth: int, //the identation depth
	comments: [dynamic] ^ast.Comment_Group,
	latest_comment_index: int,
	allocator: mem.Allocator,
}

Whitespace :: distinct byte;

newline: Whitespace = '\n';
space: Whitespace = ' ';
unindent: Whitespace = '<';
indent: Whitespace = '>';
ignore: Whitespace = '!';

lbrace: byte = '{';
rbrace: byte = '}';
lparen: byte = '(';
rparen: byte = ')';
semicolon: byte = ';';
lbracket: byte = '[';
rbracket: byte = ']';
comma: byte = ',';
dot: byte = '.';

make_printer :: proc(config: Config, allocator := context.allocator) -> Printer {
	return {
		string_builder = strings.make_builder(allocator),
		config = config,
		source_position = {
			line = 1,
			column = 1,
		},
		out_position = {
			line = 1,
			column = 1,
		},
		allocator = allocator,
		whitespaces = make([] Whitespace, 100, allocator),
	};
}

to_string :: proc(printer: Printer) -> string {
	return strings.to_string(printer.string_builder);
}

print :: proc(p: ^Printer, args: ..any) {

	for arg in args {

		data: string;
		b: byte;
		tok: tokenizer.Token;

		switch a in arg {
		case string:
			data = a;
		case tokenizer.Token:
			#partial switch a.kind {
			case .Case:
				p.depth -= 1;
			}
			tok = a;
			data = a.text;
		case byte:
			b = a;
			data = strings.string_from_ptr(&b, 1);
		case Whitespace:
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

newline_until_pos :: proc(p: ^Printer, pos: tokenizer.Pos) {

	lines := min(pos.line - p.source_position.line, p.config.newline_limit);

	for i := 0; i < lines; i += 1 {
		print(p, newline);
	}

	p.source_position = pos;
}

comment_before_position :: proc(p: ^Printer, pos: tokenizer.Pos) -> bool {

	if len(p.comments) <= p.latest_comment_index {
		return false;
	}

	comment := p.comments[p.latest_comment_index];

	return comment.pos.offset < pos.offset + p.current_whitespace;
}

next_comment_group :: proc(p: ^Printer) {
	p.latest_comment_index += 1;
}

prefix_comment_group :: proc(p: ^Printer, last: tokenizer.Pos, pos: tokenizer.Pos, comment_group: ^ast.Comment_Group) {

	//unwind all the newlines from the last position of a non whitespace to the current

	for i := 0; i < p.current_whitespace; i += 1 {

		switch w := p.whitespaces[i]; w {
		case newline:
			p.whitespaces[i] = ignore;
		}

	}

	write_whitespaces(p, p.current_whitespace);
}

postfix_comments :: proc(p: ^Printer, pos: tokenizer.Pos, last_comment: ^tokenizer.Token) {

	//after writing all the comments we need to make sure that the token we are writing is newlined correctly

	//account for multiline comments
	newlines := strings.count(last_comment.text, "\n");

	lines := min(pos.line - last_comment.pos.line - newlines, p.config.newline_limit);

	for i := 0; i < lines; i += 1 {
		write_byte(p, '\n');
	}

}

write_prefix_comment :: proc(p: ^Printer, prev_comment: ^tokenizer.Token, comment: tokenizer.Token) {

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

		for i := 0; i < lines; i += 1 {
			write_byte(p, '\n');
		}

	}

}

write_comment :: proc(p: ^Printer, comment: tokenizer.Token) {

	if comment.text[0] == '/' && comment.text[1] == '/' {
		write_string(p, comment.pos, comment.text);
	}

	else {
		write_string(p, comment.pos, comment.text);
		newlines := strings.count(comment.text, "\n");
		p.source_position.line += newlines;
		p.out_position.line += newlines;
	}
}

write_comments :: proc(p: ^Printer, pos: tokenizer.Pos) {

	next := p.last_position;
	prev_comment: ^tokenizer.Token;

	for comment_before_position(p, pos) {

		comment_group := p.comments[p.latest_comment_index];

		prefix_comment_group(p, next, pos, comment_group);

		for comment, i in comment_group.list {
			write_prefix_comment(p, prev_comment, comment);
			write_comment(p, comment);
			next = comment.pos;
			prev_comment = &comment_group.list[i];
		}

		next_comment_group(p);
	}

	if prev_comment != nil {
		postfix_comments(p, pos, prev_comment);
	}

}

write_whitespaces :: proc(p: ^Printer, n: int) {

	for i := 0; i < n; i += 1 {

		switch c := p.whitespaces[i]; c {
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

write_string :: proc(p: ^Printer, pos: tokenizer.Pos, str: string) {

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

write_byte :: proc(p: ^Printer, b: byte) {

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

write_indent :: proc(p: ^Printer) {

	if p.config.tabs {
		for i := 0; i < p.depth ; i += 1 {
			write_byte(p, '\t');
		}
	}

	else {
		for i := 0; i < p.depth * p.config.spaces; i += 1 {
			write_byte(p, ' ');
		}
	}

}

set_source_position :: proc(p: ^Printer, pos: tokenizer.Pos) {
	p.source_position = pos;
}

print_expr :: proc(p: ^Printer, expr: ^ast.Expr) {

	using ast;

	if expr == nil {
		return;
	}

	set_source_position(p, expr.pos);

	switch v in expr.derived {
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

		if v.variants != nil && (len(v.variants) == 0 || v.pos.line == v.end.line) {
			print(p, space, lbrace);
			print_exprs(p, v.variants, ", ");
			print(p, rbrace);
		}

		else {
			print(p, space, lbrace, newline);
			print_exprs(p, v.variants, ",");
			print(p, newline, rbrace);
		}
	case Enum_Type:
		print(p, "enum");

		set_source_position(p, v.fields[len(v.fields)-1].pos);

		if v.fields != nil && (len(v.fields) == 0 || v.pos.line == v.end.line) {
			print(p, space, lbrace);
			print_exprs(p, v.fields, ", ");
			print(p, rbrace);
		}

		else {
			print(p, space, lbrace, newline);
			print_exprs(p, v.fields, ",");
			print(p, newline, rbrace);
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
			print_field_list(p, v.fields, ",");
			print_end_brace(p);
		}

		set_source_position(p, v.end);
	case Proc_Lit:
		print(p, "proc"); //TOOD(ast is missing proc token)
		print(p, lparen);

		if v.type != nil {
			print_field_list(p, v.type.params, ", ");
		}

		print(p, rparen);

		if v.type != nil && v.type.results != nil {
			print(p, space, "->", space);

			if len(v.type.results.list) > 1 || (len(v.type.results.list) == 1 && v.type.results.list[0].type != nil) {
				print(p, lparen);
				print_field_list(p, v.type.results);
				print(p, rparen);
			}

			else{
				print_field_list(p, v.type.results);
			}

		}

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
	case Basic_Lit:
		print(p, v.tok);
	case Binary_Expr:
		if v.op.kind == .Ellipsis || v.op.kind == .Range_Half {
			print_expr(p, v.left);
			print(p, v.op);
			print_expr(p, v.right);
		}

		else {
			print_expr(p, v.left);
			print(p, space, v.op, space);
			print_expr(p, v.right);
		}
	case Implicit_Selector_Expr:
		print(p, dot, v.field^);
	case Call_Expr:
		print_expr(p, v.expr);
		print(p, lparen);
		print_exprs(p, v.args, ", ");
		print(p, rparen);
	case Typeid_Type:
		print(p, "type_id");
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
		if len(v.elems) == 0 {
			return;
		}

		if v.type != nil {
			print_expr(p, v.type);
			print(p, space);
		}

		set_source_position(p, v.elems[0].pos);

		if v.pos.line != v.elems[len(v.elems)-1].pos.line {
			print(p, lbrace);
			print(p, newline);
			print_exprs(p, v.elems, ", ");
			print(p, newline, rbrace);
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
		print(p, dot, lparen);
		print_expr(p, v.type);
		print(p, rparen);
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
	case:
		panic(fmt.aprint(expr.derived));
	}

}

print_exprs :: proc(p: ^Printer, list: []^ast.Expr, sep := " ") {

	if len(list) == 0 {
		return;
	}

	if list[0].pos.line == list[len(list)-1].pos.line {

		for expr, i in list {

			print_expr(p, expr);

			if i != len(list) - 1{
				print(p, sep);
			}
		}

	}

	else {

	}

}


print_field_list :: proc(p: ^Printer, list: ^ast.Field_List, sep := "") {

	if list.list == nil {
		return;
	}

	for field, i in list.list {

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

		if i != len(list.list) - 1 {
			print(p, sep);
		}
	}

}

print_stmt :: proc(p: ^Printer, stmt: ^ast.Stmt, empty_block := false) {

	using ast;

	if stmt == nil {
		return;
	}

	switch v in stmt.derived {
	case Value_Decl:
		print_decl(p, cast(^Decl)stmt);
		return;
	case Foreign_Import_Decl:
		print_decl(p, cast(^Decl)stmt);
		return;
	case Foreign_Block_Decl:
		print_decl(p, cast(^Decl)stmt);
		return;
	}

	switch v in stmt.derived {
	case Using_Stmt:
		print(p, "using", space);
		print_exprs(p, v.list, ", ");
	case Block_Stmt:

		newline_until_pos(p, v.pos);

		if v.pos.line == v.end.line {
			if !empty_block {
				print(p, lbrace);
			}

			set_source_position(p, v.pos);

			for stmt in v.stmts {
				print_stmt(p, stmt);
			}

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

			for stmt in v.stmts {
				print_stmt(p, stmt);
			}

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
			print_stmt(p, v.init);
			print(p, ";", space);
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
			print_stmt(p, v.init);
			print(p, ";", space);
		}

		print_expr(p, v.cond);

		print(p, space);

		print_stmt(p, v.body);
	case Case_Clause:

		newline_until_pos(p, v.pos);

		print(p, "case", indent);

		if v.list != nil {
			print(p, space);
			print_exprs(p, v.list, ",");
		}

		print(p, v.terminator, space);

		for stmt in v.body {
			print_stmt(p, stmt);
		}

		print(p, unindent);
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
		print_exprs(p, v.lhs, ", ");
		print(p, space, v.op, space);
		print_exprs(p, v.rhs, ", ");
	case Expr_Stmt:
		newline_until_pos(p, v.pos);
		print_expr(p, v.expr);
	case For_Stmt:

		newline_until_pos(p, v.pos);

		if v.label != nil {
			print_expr(p, v.label);
			print(p, ":", space);
		}

		print(p, "for", space);

		if v.init != nil {
			print_stmt(p, v.init);
			print(p, semicolon, space);
		}

		print_expr(p, v.cond);

		if v.post != nil {
			print(p, semicolon, space);
			print_stmt(p, v.post);
		}

		print(p, space);

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
		}

		print(p, space, "in", space);
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
		}

		print(p, space, "in", space);
		print_expr(p, v.expr);
		print(p, space);

		print_stmt(p, v.body);
	case Return_Stmt:
		newline_until_pos(p, v.pos);
		print(p, "return", space);
		print_exprs(p, v.results, ", ");
	case Defer_Stmt:
		newline_until_pos(p, v.pos);
		print(p, "defer", space);
		print_stmt(p, v.stmt);
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

	case:
		panic(fmt.aprint(stmt.derived));
	}

	set_source_position(p, stmt.end);
}

print_decl :: proc(p: ^Printer, decl: ^ast.Decl) {

	using ast;

	if decl == nil {
		return;
	}

	switch v in decl.derived {
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
		}

		else {
			newline_until_pos(p, decl.pos);
		}

		print_attributes(p, v.attributes);

		if v.is_using {
			print(p, "using", space);
		}

		print_exprs(p, v.names, ", ");

		seperator := ":";

		if !v.is_mutable {
			seperator = ":: ";
		}

		if v.type != nil {
			print(p, seperator, space);
			print_expr(p, v.type);
		}

		else {
			print(p, space, seperator);
		}

		if v.is_mutable && v.type != nil && len(v.values) != 0 {
			print(p, space, "=", space);
		}

		else if v.is_mutable && v.type == nil && len(v.values) != 0 {
			print(p, "=", space);
		}

		print_exprs(p, v.values, ", ");

	case:
		panic(fmt.aprint(decl.derived));
	}

}

print_attributes :: proc(p: ^Printer, attributes: [dynamic] ^ast.Attribute) {

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

print_file :: proc(p: ^Printer, file: ^ast.File) {

	p.comments = file.comments;

	print(p, file.pkg_token, space, file.pkg_name);

	for decl in file.decls {
		print_decl(p, cast(^ast.Decl)decl);
	}

}

print_begin_brace :: proc(p: ^Printer) {

	if p.config.bracket_style == .Allman {

		//only newline when it isn't a empty block
		if p.last_position.line == p.source_position.line {
			print(p, newline);
		}

		print(p, lbrace, indent);
	}

	else if p.config.bracket_style == ._1TBS {
		print(p, lbrace, indent);
	}

}

print_end_brace :: proc(p: ^Printer) {
	print(p, unindent, newline, rbrace);
}