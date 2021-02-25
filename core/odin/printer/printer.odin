package odin_printer

import "core:odin/ast"
import "core:odin/tokenizer"
import "core:strings"
import "core:runtime"
import "core:fmt"

Config :: struct {
    spaces: int,
    tabs: bool,
    bracket_newline: bool, //have the ability to choose for structs, enums, etc?
}

default_style := Config {
    spaces = 4,
    bracket_newline = true,
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
lbracket :: '{';
rbracket :: '}';
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
            case lbracket:
                str = "{";
                p.depth += 1;
            case rbracket:
                str = "}";
                p.depth -= 1;
            case whitespace:
                str = " ";
            }
        case ast.Ident:
            str = a.name;
        }

        if p.newline && str != "\n" {
            write_indent(p);
            p.newline = false;
        }

        if tok.kind == .Case {
            p.depth += 1;
        }

        write_string(p, str);
    }


}

print_expr :: proc(p: ^Printer, expr: ^ast.Expr) {

    using ast;

    switch v in expr.derived {
    case Ident:
        print(p, v);
    case Struct_Type:
        print(p, "struct");

        print(p, whitespace, lbracket);
        p.source_position = v.fields.pos;
        print_field_list(p, v.fields);
        print(p, newline, rbracket);
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

        print(p, whitespace, lbracket);
        p.source_position = v.body.pos;
        print_stmt(p, v.body);
        print(p, newline, rbracket);
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
    case:
        fmt.println(expr.derived);
    }

}

print_exprs :: proc(p: ^Printer, list: [] ^ ast.Expr, sep := " ") {

    for expr, i in list {

        print_expr(p, expr);

        if i != len(list) - 1{
            print(p, sep);
        }
    }

}

print_field_list :: proc(p: ^Printer, list: ^ast.Field_List) {

    if list.list == nil {
        return;
    }

    for field in list.list {
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
    }

}

print_stmt :: proc(p: ^Printer, stmt: ^ast.Stmt) {

    using ast;

    if stmt == nil {
        return;
    }

    newline_until_pos(p, stmt.pos);

    switch v in stmt.derived {
    case Block_Stmt:
        for stmt in v.stmts {
            print_stmt(p, stmt);
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

        print(p, whitespace, lbracket);
        p.source_position = v.body.pos;
        print_stmt(p, v.body);
        print(p, newline, rbracket);

        if v.else_stmt != nil {
            print(p, newline, newline, "else");

            if _, ok := v.else_stmt.derived.(If_Stmt); !ok {
                print(p, whitespace, lbracket);
                //can i defer print(p, newline, rbracket); to the outer scope?
            }

            else {
                print(p, whitespace);
            }

            p.source_position = v.else_stmt.pos;
            print_stmt(p, v.else_stmt);

            if _, ok := v.else_stmt.derived.(If_Stmt); !ok {
                print(p, newline, rbracket);
            }
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

        print(p, whitespace, lbracket);
        p.source_position = v.body.pos;
        print_stmt(p, v.body);
        print(p, newline, rbracket);
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

        print(p, whitespace, lbracket);
        p.source_position = v.body.pos;
        print_stmt(p, v.body);
        print(p, newline, rbracket);
    case Assign_Stmt:
        print_exprs(p, v.lhs, ",");
        print(p, whitespace, v.op, whitespace);
        print_exprs(p, v.rhs, ",");
    case Expr_Stmt:
        print_expr(p, v.expr);
    case:
        fmt.println(stmt.derived);
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

        seperator := " :";

        if !v.is_mutable {
            seperator = " :: ";
        }

        print(p, seperator);

        if v.type != nil {
           print_expr(p, v.type);
        }

        else if v.is_mutable {
            print(p, "=", whitespace);
        }

        print_exprs(p, v.values, ",");
    case:
        fmt.println(decl.derived);
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