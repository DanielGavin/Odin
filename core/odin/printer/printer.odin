package odin_printer

import "core:odin/ast"
import "core:odin/tokenizer"
import "core:strings"
import "core:runtime"
import "core:fmt"

Config :: struct {
	spaces: int,
	tabs: bool,
	convert_do: bool,
}

default_style := Config {
	spaces = 4,
	convert_do = false,
};


Printer :: struct {
	string_builder: strings.Builder,
	config: Config,
	source_position: tokenizer.Pos,
	out_position: tokenizer.Pos,
	depth: int,
	newline: bool,
}

newline :: '\n';
whitespace :: ' ';
lbrace :: '{';
rbrace :: '}';
lparen :: '(';
rparen :: ')';
semicolon :: ';';

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
		}
	};
}

to_string :: proc(printer: Printer) -> string {
	return strings.to_string(printer.string_builder);
}

print :: proc(p: ^Printer, args: ..any) {

	for arg in args {

		str: string;
		tok: tokenizer.Token;

		switch a in arg {
		case string:
			str = a;
		case tokenizer.Token:
			#partial switch a.kind {
			case .Case:
				p.depth -= 1;
			}
			tok = a;
			str = a.text;
		case rune:
			switch a  {
			case newline:
				p.out_position.line += 1;
				p.out_position.column = 1;
				p.source_position.line += 1;
				p.source_position.column = 1;
				p.newline = true;
				str = "\n";
			case lparen:
				str = "(";
			case rparen:
				str = ")";
			case lbrace:
				str = "{";
			case rbrace:
				str = "}";
				p.depth -= 1;
			case whitespace:
				str = " ";
			case semicolon:
				str = ";";
			}
		case ast.Ident:
			str = a.name;
		}

		if p.newline && str != "\n" {
			write_indent(p);
			p.newline = false;
		}

		if str == "{" {
			p.depth += 1;
		}

		if tok.kind == .Case {
			p.depth += 1;
		}

		write_string(p, str);
	}


}

print_expr :: proc(p: ^Printer, expr: ^ast.Expr) {

	using ast;

	if expr == nil {
		return;
	}

	switch v in expr.derived {
	case Ident:
		print(p, v);
	case Struct_Type:
		print(p, "struct");

		if v.poly_params != nil {
			print(p, lparen);
			print_field_list(p, v.poly_params, ", ");
			print(p, rparen);
		}

		p.source_position = v.fields.pos;
		print(p, whitespace, lbrace, newline);
		print_field_list(p, v.fields, ",");
		print(p, newline, rbrace);
	case Proc_Lit:
		print(p, "proc"); //TOOD(ast is missing proc token)
		print(p, lparen);

		if v.type != nil {
			print_field_list(p, v.type.params);
		}

		print(p, rparen);

		if v.type != nil && v.type.results != nil {
			print(p, whitespace, "->", whitespace);

			if len(v.type.results.list) > 1 {
				print(p, lparen);
			}

			print_field_list(p, v.type.results);

			if len(v.type.results.list) > 1 {
			   print(p, rparen);
			}

		}

		p.source_position = v.body.pos;

		print(p, whitespace);

		print_stmt(p, v.body);
	case Basic_Lit:
		print(p, v.tok);
	case Binary_Expr:
		print_expr(p, v.left);
		print(p, whitespace, v.op, whitespace);
		print_expr(p, v.right);
	case Implicit_Selector_Expr:
		print(p, ".", v.field^);
	case Call_Expr:
		print_expr(p, v.expr);
		print(p, lparen);
		print_exprs(p, v.args, ", ");
		print(p, rparen);
	case Typeid_Type:
		print(p, "type_id");
	case Selector_Expr:
		print_expr(p, v.expr);
		print(p, ".");
		print_expr(p, v.field);
	case:
		fmt.println(expr.derived);
		fmt.println();
	}

}

print_exprs :: proc(p: ^Printer, list: [] ^ ast.Expr, sep := " ") {

	for expr, i in list {

		newline_until_pos(p, expr.pos);

		print_expr(p, expr);

		if i != len(list) - 1{
			print(p, sep);
		}
	}

}

print_field_list :: proc(p: ^Printer, list: ^ast.Field_List, sep := "") {

	if list.list == nil {
		return;
	}

	for field, i in list.list {

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

	newline_until_pos(p, stmt.pos);

	switch v in stmt.derived {
	case Block_Stmt:

		if !empty_block {
			print(p, lbrace, newline);
		}

		for stmt in v.stmts {
			print_stmt(p, stmt);
		}

		if !empty_block {
			print(p, newline, rbrace);
		}
	case Value_Decl:
		print_decl(p, cast(^Decl)stmt);
	case If_Stmt:

		if v.label != nil {
			print_expr(p, v.label);
			print(p, ":", whitespace);
		}

		print(p, "if", whitespace);

		if v.init != nil {
			print_stmt(p, v.init);
			print(p, ";", whitespace);
		}

		print_expr(p, v.cond);


		p.source_position = v.body.pos;

		uses_do := false;

		if check_stmt, ok := v.body.derived.(Block_Stmt); ok && check_stmt.uses_do {
			uses_do = true;
		}

		if uses_do && !p.config.convert_do {
			print(p, whitespace, "do", whitespace);
			print_stmt(p, v.body, true);
			p.source_position = v.body.end;
		}

		else {
			if uses_do {
				print(p, newline);
			}

			print(p, whitespace);

			print_stmt(p, v.body);

			p.source_position = v.body.end;
		}

		if v.else_stmt != nil {
			print(p, newline, newline, "else");
			print(p, whitespace);

			p.source_position = v.else_stmt.pos;

			print_stmt(p, v.else_stmt);

		}
	case Switch_Stmt:

		if v.label != nil {
			print_expr(p, v.label);
			print(p, ":", whitespace);
		}

		print(p, "switch", whitespace);

		if v.init != nil {
			print_stmt(p, v.init);
			print(p, ";", whitespace);
		}

		print_expr(p, v.cond);

		p.source_position = v.body.pos;

		print(p, whitespace);

		print_stmt(p, v.body);
	case Case_Clause:
		token := tokenizer.Token {
			text = "case",
			kind = .Case,
			pos = v.case_pos,
		};

		print(p, token, whitespace);
		print_exprs(p, v.list, ",");
		print(p, v.terminator);

		for stmt in v.body {
			print_stmt(p, stmt);
		}
	case Type_Switch_Stmt:
		if v.label != nil {
			print_expr(p, v.label);
			print(p, ":", whitespace);
		}

		print(p, "switch", whitespace);

		print_stmt(p, v.tag);

		p.source_position = v.body.pos;

		print(p, whitespace);

		print_stmt(p, v.body);
	case Assign_Stmt:
		print_exprs(p, v.lhs, ", ");
		print(p, whitespace, v.op, whitespace);
		print_exprs(p, v.rhs, ", ");
	case Expr_Stmt:
		print_expr(p, v.expr);
	case For_Stmt:

		if v.label != nil {
			print_expr(p, v.label);
			print(p, ":", whitespace);
		}

		print(p, "for", whitespace);

		print_stmt(p, v.init);
		print(p, semicolon, whitespace);
		print_expr(p, v.cond);
		print(p, semicolon, whitespace);
		print_stmt(p, v.post);

		p.source_position = v.body.pos;

		print_stmt(p, v.body);
	case:
		fmt.println(stmt.derived);
		fmt.println();
	}

}

print_decl :: proc(p: ^Printer, decl: ^ast.Decl) {

	using ast;

	if decl == nil {
		return;
	}

	newline_until_pos(p, decl.pos);

	switch v in decl.derived {
	case Import_Decl:
		if v.name.text != "" {
			print(p, v.import_tok, " ", v.name, " ", v.fullpath);
		}

		else {
			print(p, v.import_tok, " ", v.fullpath);
		}
	case Value_Decl:
		print_exprs(p, v.names, ",");

		seperator := ":";

		if !v.is_mutable {
			seperator = " :: ";
		}

		if v.type != nil {
			print(p, seperator, whitespace);
			print_expr(p, v.type);
		}

		else {
			print(p, whitespace, seperator);
		}

		if v.is_mutable && v.type != nil {
			print(p, whitespace, "=", whitespace);
		}

		else if v.is_mutable && v.type == nil {
			print(p, "=", whitespace);
		}

		print_exprs(p, v.values, ",");
	case:
		fmt.println(decl.derived);
		fmt.println();
	}


}

print_file :: proc(p: ^Printer, file: ^ast.File) {

	print(p, file.pkg_token, whitespace, file.pkg_name);

	for decl in file.decls {
		print_decl(p, cast(^ast.Decl)decl);
	}


}

newline_until_pos :: proc(p: ^Printer, pos: tokenizer.Pos) {

	for i := p.source_position.line; i < pos.line; i += 1 {
		print(p, newline);
	}

	p.source_position.line = pos.line;
}

write_string :: proc(p: ^Printer, str: string) {
	strings.write_string(&p.string_builder, str);
}

write_indent :: proc(p: ^Printer) {

	//this is stupid

	if p.config.tabs {
		for i := 0; i < p.depth ; i += 1 {
			write_string(p, "\t");
		}
	}

	else {
		for i := 0; i < p.depth * p.config.spaces; i += 1 {
			write_string(p, " ");
		}
	}

}